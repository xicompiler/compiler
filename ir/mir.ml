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
  [ `Func of label * stmt list
  | `Data of label * int64
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

(** [to_addr_expr p m] is [p + m * 8] as an IR expression *)
let to_addr_expr p m = `Bop (`Plus, p, `Bop (`Mult, m, eight))

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
  | `Int i -> i
  | `Bool b -> b |> Bool.to_int |> Int64.of_int
  | `Char c -> c |> Uchar.to_scalar |> Int64.of_int

(** [translate_primitive id] is the mir representation of [id] *)
let translate_id id = `Temp (PosNode.get id)

(** [translate_alloc space] is the mir representation of a call to
    allocate space *)
let translate_alloc space = `Call (0, xi_alloc, [ space ])

(** [translate_expr enode] is the mir representation of expression node
    [enode] *)
let rec translate_expr ~gensym enode =
  let ctx = DecNode.Expr.context enode in
  match DecNode.Expr.get enode with
  | Primitive p -> `Const (translate_primitive p)
  | Id id -> translate_id id
  | Array arr -> translate_arr ~gensym arr
  | String s -> translate_string ~gensym s
  | Bop (op, e1, e2) -> translate_bop ~gensym op e1 e2
  | Uop (op, e) -> translate_uop ~gensym op e
  | FnCall (id, es) -> (translate_call ~gensym ~ctx id es :> expr)
  | Length e -> translate_length ~gensym e
  | Index (e1, e2) -> translate_index ~gensym e1 e2

(** [translate_exprs es] is [es], each of which translated to IR *)
and translate_exprs ~gensym es = List.map ~f:(translate_expr ~gensym) es

(** [translate_arr arr] is the mir representation of [arr] *)
and translate_arr ~gensym arr =
  arr |> translate_exprs ~gensym |> translate_arr_lst ~gensym

(** [translate_arr_lst lst] is the mir representation of a list [lst] *)
and translate_arr_lst ~gensym lst =
  let n = length lst in
  let tm = Temp.fresh gensym in
  let f (len, acc) e =
    let index = to_addr len in
    (succ len, `Move (`Mem (`Bop (`Plus, tm, `Const index)), e) :: acc)
  in
  let add_elts =
    lst |> List.fold ~f ~init:(Int64.zero, []) |> snd |> List.rev
  in
  let alloc_arr = `Move (tm, translate_alloc (`Const (to_addr n))) in
  let add_len = `Move (`Mem tm, `Const n) in
  `ESeq
    (`Seq (alloc_arr :: add_len :: add_elts), `Bop (`Plus, tm, eight))

(** [translate_string str] is the mir representation of [str] *)
and translate_string ~gensym str =
  let f s = `Const (s |> int_of_char |> Int64.of_int) in
  str |> String.to_list_rev |> List.rev_map ~f
  |> translate_arr_lst ~gensym

(** [translate_uop uop e] is the mir representation of unary operator
    expression [uop e] *)
and translate_uop ~gensym uop e =
  match uop with
  | `IntNeg -> translate_int_neg ~gensym e
  | `LogNeg -> e |> translate_expr ~gensym |> log_neg

(** [translate_int_neg e] is the mir representation of an integer
    negation of [e] *)
and translate_int_neg ~gensym e =
  match DecNode.Expr.get e with
  | Primitive (`Int i) -> `Const ~-i
  | _ -> `Bop (`Minus, zero, translate_expr ~gensym e)

(** [translate_call ctx id es] is the mir representation of a function
    call with function id [id], arguments [es], and context [ctx] *)
and translate_call ~gensym ~ctx id es : expr Subtype.call =
  let name = mangle id ~ctx in
  let rets = num_returns id ~ctx in
  `Call (rets, `Name name, translate_exprs ~gensym es)

(** [translate_bop bop e1 e2] is the mir representation of binary
    operator expression [bop e1 e2] *)
and translate_bop ~gensym bop e1 e2 =
  match (bop, DecNode.Expr.get e1, DecNode.Expr.get e2) with
  | `And, _, _ -> translate_and ~gensym e1 e2
  | `Or, _, _ -> translate_or ~gensym e1 e2
  | `Plus, Array a1, Array a2 -> translate_arr ~gensym (a1 @ a2)
  | #Ast.Op.binop, _, _ ->
      let e1 = translate_expr ~gensym e1 in
      let e2 = translate_expr ~gensym e2 in
      `Bop (Op.coerce bop, e1, e2)

(** [translate_and e1 e2] is the mir representation of [e1 and e2] *)
and translate_and ~gensym e1 e2 =
  let x = Temp.fresh gensym in
  let l1, l2, lf = Label.fresh3 gensym in
  `ESeq
    ( `Seq
        [
          `Move (x, zero);
          translate_control ~gensym e1 l1 lf;
          `Label l1;
          translate_control ~gensym e2 l2 lf;
          `Label l2;
          `Move (x, one);
          `Label lf;
        ],
      x )

(** [translate_or e1 e2] is the mir representation of [e1 or e2] *)
and translate_or ~gensym e1 e2 =
  let x = Temp.fresh gensym in
  let l1, l2, lt = Label.fresh3 gensym in
  `ESeq
    ( `Seq
        [
          `Move (x, one);
          translate_control ~gensym e1 lt l1;
          `Label l1;
          translate_control ~gensym e2 lt l2;
          `Label l2;
          `Move (x, zero);
          `Label lt;
        ],
      x )

(** [translate_length e] is the mir representation of a length function
    call with argument [e] *)
and translate_length ~gensym e =
  `Mem (`Bop (`Minus, translate_expr ~gensym e, eight))

(** [translate_index e1 e2] is the mir representation of an indexing of
    [e1] at position [e2] *)
and translate_index ~gensym e1 e2 =
  let ta, ti = Temp.fresh2 gensym in
  let lok, lerr = Label.fresh2 gensym in
  `ESeq
    ( `Seq
        [
          `Move (ta, translate_expr ~gensym e1);
          `Move (ti, translate_expr ~gensym e2);
          `CJump
            (`Bop (`ULt, ti, `Mem (`Bop (`Minus, ta, eight))), lok, lerr);
          `Label lerr;
          ir_pr_call xi_out_of_bounds;
          `Label lok;
        ],
      `Mem (to_addr_expr ta ti) )

(** [translate_stmt snode] is the mir representation of statement node
    [snode] *)
and translate_stmt ~gensym snode =
  let ctx = DecNode.Stmt.context snode in
  match DecNode.Stmt.get snode with
  | If (e, s) -> translate_if ~gensym e s
  | IfElse (e, s1, s2) -> translate_if_else ~gensym e s1 s2
  | While (e, s) -> translate_while ~gensym e s
  | VarDecl _ -> empty
  | ArrayDecl (id, _, es) -> translate_array_decl ~gensym id es
  | Assign (id, e) -> translate_assign ~gensym id e
  | ArrAssign (e1, e2, e3) -> translate_arr_assign ~gensym e1 e2 e3
  | ExprStmt (id, es) | PrCall (id, es) ->
      (translate_call ~gensym ~ctx id es :> stmt)
  | VarInit (id, _, e) -> translate_assign ~gensym id e
  | MultiAssign (ds, id, es) ->
      translate_multi_assign ~gensym ~ctx ds id es
  | Return es -> translate_return ~gensym es
  | Block stmts -> translate_block ~gensym stmts

(** [translate_if_stmt e s] is [stmts, f] where [stmts] is the first
    three IR instructions in if and if else statements, reversed, and
    [f] is the false label *)
and translate_if_stmt ~gensym e s =
  let t, f = Label.fresh2 gensym in
  let s = translate_stmt ~gensym s in
  ([ s; `Label t; translate_control ~gensym e t f ], f)

(** [translate_if e s] is the mir representation of an if statement with
    condition [e] and body [s] *)
and translate_if ~gensym e s =
  let stmt, f = translate_if_stmt ~gensym e s in
  stmt |> List.cons (`Label f) |> List.rev |> seq

(** [translate_if_else e s1 s2] is the mir representation of an if-else
    statement with condition [e] and statements [s1] and [s2] *)
and translate_if_else ~gensym e s1 s2 =
  let l_end = Label.fresh gensym in
  let stmt, f = translate_if_stmt ~gensym e s1 in
  let open List in
  stmt
  |> cons (`Jump (`Name l_end))
  |> cons (`Label f)
  |> cons (translate_stmt ~gensym s2)
  |> cons (`Label l_end)
  |> rev |> seq

(** [translate_while e s] is the mir representation of a while loop with
    condition [e] and loop body [s] *)
and translate_while ~gensym e s =
  let header, t, f = Label.fresh3 gensym in
  `Seq
    [
      `Label header;
      translate_control ~gensym e t f;
      `Label t;
      translate_stmt ~gensym s;
      `Jump (`Name header);
      `Label f;
    ]

(** [translate_array_decl_shallow len] is [stmts, temp] where [stmts] is
    the first three IR instructions in array decl statements, reversed,
    and [temp] is the memory address of the allocated space *)
and translate_array_decl_shallow ~gensym len =
  let e = translate_expr ~gensym len in
  let tn, tm = Temp.fresh2 gensym in
  ( [
      `Move (`Mem tm, tn);
      `Move (tm, translate_alloc (to_addr_expr eight tn));
      `Move (tn, e);
    ],
    tm )

(** [translate_array_decl id es] is the mir representation of an array
    declaration for variable [id] with lengths [es] *)
and translate_array_decl ~gensym id = function
  | Some e :: es ->
      let name = `Temp (PosNode.get id) in
      let shallow, tm = translate_array_decl_shallow ~gensym e in
      shallow
      |> List.cons (`Move (name, `Bop (`Plus, tm, eight)))
      |> List.rev |> seq
  | None :: _ | [] -> empty

(** [translate_assign id e] is the mir representation of an assignment
    of [e] to [id] *)
and translate_assign ~gensym id e =
  `Move (`Temp (PosNode.get id), translate_expr ~gensym e)

(** [translate_arr_assign e1 e2 e3] is the mir representation of a array
    assignment statement with *)
and translate_arr_assign ~gensym e1 e2 e3 =
  let ta, ti = Temp.fresh2 gensym in
  let lok, lerr = Label.fresh2 gensym in
  `Seq
    [
      `Move (ta, translate_expr ~gensym e1);
      `Move (ti, translate_expr ~gensym e2);
      `CJump
        (`Bop (`ULt, ti, `Mem (`Bop (`Minus, ta, eight))), lok, lerr);
      `Label lerr;
      ir_pr_call xi_out_of_bounds;
      `Label lok;
      `Move (`Mem (to_addr_expr ta ti), translate_expr ~gensym e3);
    ]

(** [translate_multi_assign ctx ds id es] is the mir representation of a
    multi-assignment with declarations [ds], and function [id], and
    arguments [es] *)
and translate_multi_assign ~gensym ~ctx ds id es =
  let name = mangle id ~ctx in
  let rets = num_returns id ~ctx in
  let expr_lst = translate_exprs ~gensym es in
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
and translate_return ~gensym es = `Return (translate_exprs ~gensym es)

(** [translate_block stmts] is the mir representation of a statement
    block with statements [stmts] *)
and translate_block ~gensym stmts =
  `Seq (List.map ~f:(translate_stmt ~gensym) stmts)

(** [translate_control enode t f] translates booleans to control flow *)
and translate_control ~gensym enode t f =
  match DecNode.Expr.get enode with
  | Primitive (`Bool b) -> `Jump (`Name (if b then t else f))
  | Uop (`LogNeg, e) -> translate_control ~gensym e f t
  | Bop (`And, e1, e2) ->
      let label = Label.fresh gensym in
      `Seq
        [
          translate_control ~gensym e1 label f;
          `Label label;
          translate_control ~gensym e2 t f;
        ]
  | Bop (`Or, e1, e2) ->
      let label = Label.fresh gensym in
      `Seq
        [
          translate_control ~gensym e1 t label;
          `Label label;
          translate_control ~gensym e2 t f;
        ]
  | _ -> `CJump (translate_expr ~gensym enode, t, f)

(** [returns_unit ctx id] is true if the function associated with [id]
    in [ctx] has a return of type Unit, false otherwise *)
let returns_unit ~ctx id =
  match Context.find_fn_exn ~id ctx with _, `Unit -> true | _ -> false

(** [translate_fn_defn sign] is the mir representation of a function
    definition with signature [sign] *)
let translate_fn_defn ~gensym ~ctx ({ id } : Ast.signature) block =
  let name = mangle id ~ctx in
  let block = translate_block ~gensym block in
  let body =
    if returns_unit ~ctx id then [ block; translate_return ~gensym [] ]
    else [ block ]
  in
  `Func (name, body)

(** [translate_global_init id typ p] is the mir representation of a
    global initialization of [id] with type [typ] and primitive [p] as
    its value *)
let translate_global_init id p =
  `Data (PosNode.get id, translate_primitive p)

(** [translate_defn def] is the mir representation of source toplevel
    definition [def] *)
let translate_defn ~gensym def =
  match DecNode.Toplevel.get def with
  | FnDefn (signature, block) ->
      let ctx = DecNode.Toplevel.context def in
      Some (translate_fn_defn ~gensym ~ctx signature block :> toplevel)
  | GlobalDecl _ -> None
  | GlobalInit (id, _, p) -> Some (translate_global_init id p)

let translate ~gensym { uses; definitions } =
  List.filter_map ~f:(translate_defn ~gensym) definitions
