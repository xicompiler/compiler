open Core
open Int64
open Subtype
module PosNode = Node.Position
module DecNode = Context.Node.Decorated
open Ast.Op
open Ast.Decorated
open Expr
open Stmt
open Toplevel
open Primitive
open Type
open IrGensym

type expr =
  [ expr Subtype.expr
  | expr Subtype.call
  | `ESeq of stmt * expr
  ]

and stmt =
  [ expr Subtype.cjump2
  | `Seq of stmt list
  ]

type toplevel =
  [ `Func of Subtype.label * stmt list
  | `Data of Subtype.label * Int64.t
  ]

(** [seq lst] is [`Seq lst] *)
let seq lst = `Seq lst

(** [empty] is an empty sequence of statements *)
let empty : stmt = `Seq []

(** [xi_alloc] is the xi function for allocating space *)
let xi_alloc = `Name "_xi_alloc"

(** [xi_out_of_bounds] is the xi function for an out-of-bounds array
    index *)
let xi_out_of_bounds = `Name "_xi_out_of_bounds"

let ir_pr_call name = `Call (0, name, [])

(** [length lst] is the length of a list represented in int64 *)
let length lst = lst |> List.length |> Int64.of_int

(** [to_addr n] is [n] converted to a memory address *)
let to_addr n = (8L * n) + 8L

let gensym = IrGensym.create ()

(** [encode_tau t] is [t] encoded for function mangling *)
let rec encode_tau = function
  | `Int -> "i"
  | `Bool -> "b"
  | `Array t -> "a" ^ encode_tau t
  | `Poly -> failwith "Unexpected <poly>"

(** [encode_expr t] is [t] encoded for function mangling *)
let encode_expr = function
  | `Tuple ts ->
      let len = ts |> List.length |> string_of_int in
      let types = ts |> List.map ~f:encode_tau |> String.concat in
      "t" ^ len ^ types
  | #Tau.t as t -> encode_tau t

(** [encode_term t] is [t] encoded for function mangling *)
let encode_term = function
  | `Unit -> "p"
  | #Expr.t as t -> encode_expr t

(** [encode_name s] is [s] encoded for function mangling *)
let encode_name = String.substr_replace_all ~pattern:"_" ~with_:"__"

(** [encode_args args] is [args] encoded for function mangling *)
let encode_args args = args |> List.map ~f:encode_expr |> String.concat

(** [mangle_fmt] is the format used to mangle identifiers *)
let mangle_fmt = format_of_string "_I%s_%s%s"

(** [mangle id ctx] is the mangled function name of [id] in context
    [ctx] *)
let mangle id ~ctx =
  let arg, ret = Context.find_fn_exn ~id ctx in
  let name = id |> PosNode.get |> encode_name in
  let return = encode_term ret in
  let args = arg |> tau_list_of_term |> encode_args in
  Printf.sprintf mangle_fmt name return args

(** [num_returns id ctx] is the number of values that the function [id]
    returns. *)
let num_returns id ~ctx =
  let arg, ret = Context.find_fn_exn ~id ctx in
  ret |> tau_list_of_term |> List.length

(** [translate_primitive p] is the mir representation of primitive [p] *)
let translate_primitive = function
  | `Int i -> `Const i
  | `Bool b -> if b then one else zero
  | `Char c -> `Const (c |> Uchar.to_scalar |> Int64.of_int)

(** [translate_primitive id] is the mir representation of [id] *)
let translate_id id = `Temp (PosNode.get id)

(** [translate_expr enode] is the mir representation of expression node
    [enode] *)
let rec translate_expr enode =
  let ctx = DecNode.Expr.context enode in
  match DecNode.Expr.get enode with
  | Primitive p -> translate_primitive p
  | Id id -> translate_id id
  | Array arr -> translate_arr arr
  | String s -> translate_string s
  | Bop (op, e1, e2) -> translate_bop op e1 e2
  | Uop (op, e) -> translate_uop op e
  | FnCall (id, es) -> (translate_call ~ctx id es :> expr)
  | Length e -> translate_length e
  | Index (e1, e2) -> translate_index e1 e2

(** [translate_exprs es] is [es], each of which translated to IR *)
and translate_exprs es = List.map ~f:translate_expr es

(** [translate_arr arr] is the mir representation of [arr] *)
and translate_arr arr = arr |> translate_exprs |> translate_arr_lst

(** [translate_arr_lst lst] is the mir representation of a list [lst] *)
and translate_arr_lst lst =
  let n = length lst in
  let tm = Temp.fresh gensym in
  let f (len, acc) e =
    let index = to_addr len in
    ( Int64.succ len,
      `Move (`Mem (`Bop (`Plus, tm, `Const index)), e) :: acc )
  in
  let add_elts =
    lst |> List.fold ~f ~init:(Int64.zero, []) |> snd |> List.rev
  in
  let alloc_arr =
    `Move (tm, `Call (0, xi_alloc, [ `Const (to_addr n) ]))
  in
  let add_len = `Move (`Mem tm, `Const n) in
  `ESeq
    (`Seq (alloc_arr :: add_len :: add_elts), `Bop (`Plus, tm, eight))

(** [translate_string str] is the mir representation of [str] *)
and translate_string str =
  let f s = `Const (s |> int_of_char |> Int64.of_int) in
  str |> String.to_list_rev |> List.rev_map ~f |> translate_arr_lst

(** [translate_uop uop e] is the mir representation of unary operator
    expression [uop e] *)
and translate_uop uop e =
  match uop with
  | `IntNeg -> translate_int_neg e
  | `LogNeg -> e |> translate_expr |> log_neg

(** [translate_int_neg e] is the mir representation of an integer
    negation of [e] *)
and translate_int_neg e =
  match DecNode.Expr.get e with
  | Primitive (`Int i) -> `Const ~-i
  | _ -> `Bop (`Minus, zero, translate_expr e)

(** [translate_call ctx id es] is the mir representation of a function
    call with function id [id], arguments [es], and context [ctx] *)
and translate_call ~ctx id es : expr Subtype.call =
  let name = mangle id ~ctx in
  let rets = num_returns id ~ctx in
  `Call (rets, `Name name, translate_exprs es)

(** [translate_bop bop e1 e2] is the mir representation of binary
    operator expression [bop e1 e2] *)
and translate_bop bop e1 e2 =
  match bop with
  | `And -> translate_and e1 e2
  | `Or -> translate_or e1 e2
  | #Ast.Op.binop ->
      `Bop (Op.coerce bop, translate_expr e1, translate_expr e2)

(** [translate_and e1 e2] is the mir representation of [e1 and e2] *)
and translate_and e1 e2 =
  let x = Temp.fresh gensym in
  let l1, l2, lf = Label.fresh3 gensym in
  `ESeq
    ( `Seq
        [
          `Move (x, zero);
          translate_control e1 l1 lf;
          `Label l1;
          translate_control e2 l2 lf;
          `Label l2;
          `Move (x, one);
          `Label lf;
        ],
      x )

(** [translate_or e1 e2] is the mir representation of [e1 or e2] *)
and translate_or e1 e2 =
  let x = Temp.fresh gensym in
  let l1, l2, lt = Label.fresh3 gensym in
  `ESeq
    ( `Seq
        [
          `Move (x, one);
          translate_control e1 lt l1;
          `Label l1;
          translate_control e2 lt l2;
          `Label l2;
          `Move (x, zero);
          `Label lt;
        ],
      x )

(** [translate_length e] is the mir representation of a length function
    call with argument [e] *)
and translate_length e = `Mem (`Bop (`Minus, translate_expr e, eight))

(** [translate_index e1 e2] is the mir representation of an indexing of
    [e1] at position [e2] *)
and translate_index e1 e2 =
  let ta, ti = Temp.fresh2 gensym in
  let lok, lerr = Label.fresh2 gensym in
  `ESeq
    ( `Seq
        [
          `Move (ta, translate_expr e1);
          `Move (ti, translate_expr e2);
          `CJump
            (`Bop (`ULt, ti, `Mem (`Bop (`Minus, ta, eight))), lok, lerr);
          `Label lerr;
          ir_pr_call xi_out_of_bounds;
          `Label lok;
        ],
      `Mem (`Bop (`Plus, ta, `Bop (`Mult, ti, eight))) )

(** [translate_stmt snode] is the mir representation of statement node
    [snode] *)
and translate_stmt snode =
  let ctx = DecNode.Stmt.context snode in
  match DecNode.Stmt.get snode with
  | If (e, s) -> translate_if e s
  | IfElse (e, s1, s2) -> translate_if_else e s1 s2
  | While (e, s) -> translate_while e s
  | VarDecl _ | ArrayDecl _ -> empty
  | Assign (id, e) -> translate_assign id e
  | ArrAssign (e1, e2, e3) -> translate_arr_assign e1 e2 e3
  | ExprStmt (id, es) | PrCall (id, es) ->
      (translate_call ~ctx id es :> stmt)
  | VarInit (id, _, e) -> translate_assign id e
  | MultiAssign (ds, id, es) -> translate_multi_assign ~ctx ds id es
  | Return es -> translate_return es
  | Block stmts -> translate_block stmts

(** [translate_if_stmt e s] is the first three IR instructions in if and
    if else statements, reversed*)
and translate_if_stmt e s =
  let t, f = Label.fresh2 gensym in
  [ translate_stmt s; `Label t; translate_control e t f ]

(** [translate_if e s] is the mir representation of an if statement with
    condition [e] and body [s] *)
and translate_if e s =
  let f = Label.fresh gensym in
  s |> translate_if_stmt e |> List.cons (`Label f) |> List.rev |> seq

(** [translate_if_else e s1 s2] is the mir representation of an if-else
    statement with condition [e] and statements [s1] and [s2] *)
and translate_if_else e s1 s2 =
  let f, l_end = Label.fresh2 gensym in
  let open List in
  s1 |> translate_if_stmt e
  |> cons (`Jump (`Name l_end))
  |> cons (`Label f)
  |> cons (translate_stmt s2)
  |> cons (`Label l_end)
  |> rev |> seq

(** [translate_while e s] is the mir representation of a while loop with
    condition [e] and loop body [s] *)
and translate_while e s =
  let header, t, f = Label.fresh3 gensym in
  `Seq
    [
      `Label header;
      translate_control e t f;
      `Label t;
      translate_stmt s;
      `Jump (`Name header);
      `Label f;
    ]

(** [translate_assign id e] is the mir representation of an assignment
    of [e] to [id] *)
and translate_assign id e =
  `Move (`Temp (PosNode.get id), translate_expr e)

(** [translate_arr_assign e1 e2 e3] is the mir representation of a array
    assignment statement with *)
and translate_arr_assign e1 e2 e3 =
  let ta, ti = Temp.fresh2 gensym in
  let lok, lerr = Label.fresh2 gensym in
  `Seq
    [
      `Move (ta, translate_expr e1);
      `Move (ti, translate_expr e2);
      `CJump
        (`Bop (`ULt, ti, `Mem (`Bop (`Minus, ta, eight))), lok, lerr);
      `Label lerr;
      ir_pr_call xi_out_of_bounds;
      `Label lok;
      `Move
        ( `Mem (`Bop (`Plus, ta, `Bop (`Mult, ti, eight))),
          translate_expr e3 );
    ]

(** [translate_multi_assign ctx ds id es] is the mir representation of a
    multi-assignment with declarations [ds], and function [id], and
    arguments [es] *)
and translate_multi_assign ~ctx ds id es =
  let name = mangle id ~ctx in
  let rets = num_returns id ~ctx in
  let expr_lst = translate_exprs es in
  let call = `Call (rets, `Name name, expr_lst) in
  let f (rv, acc) = function
    | None -> (Int.succ rv, acc)
    | Some (v, _) ->
        let t = "_RV" ^ Int.to_string rv in
        let lst = `Move (`Temp (PosNode.get v), `Temp t) :: acc in
        (Int.succ rv, lst)
  in
  let assign = ds |> List.fold ~f ~init:(1, []) |> snd |> List.rev in
  `Seq (call :: assign)

(** [translate_return es] is the mir representation of a return
    statement with expressions [es] *)
and translate_return es = `Return (translate_exprs es)

(** [translate_block stmts] is the mir representation of a statement
    block with statements [stmts] *)
and translate_block stmts = `Seq (List.map ~f:translate_stmt stmts)

(** [translate_control enode t f] translates booleans to control flow *)
and translate_control enode t f =
  match DecNode.Expr.get enode with
  | Primitive (`Bool b) -> `Jump (`Name (if b then t else f))
  | Uop (`LogNeg, e) -> translate_control e f t
  | Bop (`And, e1, e2) ->
      let label = Label.fresh gensym in
      translate_short_circuit e1 e2 label f t f
  | Bop (`Or, e1, e2) ->
      let label = Label.fresh gensym in
      translate_short_circuit e1 e2 t label t f
  | _ -> `CJump (translate_expr enode, t, f)

(** [translate_short_circuit e1 e2 l1 l2 t f] translates the short
    circuit operators [`And] or [`Or] to control flow *)
and translate_short_circuit e1 e2 l1 l2 t f =
  let label = Label.fresh gensym in
  `Seq
    [
      translate_control e1 l1 l2; `Label label; translate_control e2 t f;
    ]

(** [translate_fn_defn sign] is the mir representation of a function
    definition with signature [sign] *)
let translate_fn_defn ~ctx ({ id } : Ast.signature) block =
  let name = mangle id ~ctx in
  `Func (name, [ translate_block block ])

(** [translate_global_init id typ p] is the mir representation of a
    global initialization of [id] with type [typ] and primitive [p] as
    its value *)
let translate_global_init id p =
  match translate_primitive p with
  | `Const i -> `Data ("_" ^ PosNode.get id, i)
  | _ -> failwith "Primitive not a const"

(** [translate_defn def] is the mir representation of source toplevel
    definition [def] *)
let translate_defn def =
  match DecNode.Toplevel.get def with
  | FnDefn (signature, block) ->
      let ctx = DecNode.Toplevel.context def in
      Some (translate_fn_defn ~ctx signature block :> toplevel)
  | GlobalDecl _ -> None
  | GlobalInit (id, _, p) -> Some (translate_global_init id p)

let translate { uses; definitions } =
  List.filter_map ~f:translate_defn definitions
