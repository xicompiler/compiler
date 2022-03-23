open Subtype
open Core
open Int64
module PosNode = Node.Position
module DecNode = Context.Node.Decorated
open Ast.Op
open Ast.Decorated
open Expr
open Stmt
open Toplevel
open Primitive
open Type

type expr =
  [ expr Subtype.expr
  | expr Subtype.call
  | `ESeq of stmt * expr
  ]

and stmt =
  [ expr Subtype.cjump2
  | `Seq of stmt list
  ]

(** [one] is an mir expression representing the constant one *)
let one = `Const one

(** [zero] is an mir expression representing the constant zero *)
let zero = `Const zero

(** [eight] is an mir expression representing the constant eight *)
let eight = `Const 8L

(** [xi_alloc] is the xi function for allocating space *)
let xi_alloc = `Name "_xi_alloc"

(** [xi_out_of_bounds] is the xi function for an out-of-bounds array
    index *)
let xi_out_of_bounds = `Name "_xi_out_of_bounds"

let ir_pr_call name = `Call (name, [])

(** [length lst] is the length of a list represented in int64 *)
let length lst = lst |> List.length |> Int64.of_int

(** [to_addr n] is [n] converted to a memory address *)
let to_addr n = (8L * n) + 8L

(** [fresh_label ()] generates a unique label with prefix "l" *)
let fresh_label = Label.generator ()

(** [fresh_label2] is a pair of fresh labels *)
let fresh_label2 () = Thunk.map2 fresh_label

(** [fresh_label3 ()] is a triple of fresh labels *)
let fresh_label3 () = Thunk.map3 fresh_label

(** [fresh_temp ()] generates a unique temp with prefix "x" *)
let fresh_temp = Temp.generator ()

(** [fresh_temp2 ()] is a pair of fresh temporaries *)
let fresh_temp2 () = Thunk.map2 fresh_temp

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

(** [mangle id ctx] is the mangled function name of [id] in context
    [ctx] *)
let mangle id ~ctx =
  let arg, ret = Context.find_fn_exn ~id ctx in
  let name = id |> PosNode.get |> encode_name in
  let return = encode_term ret in
  let args = arg |> tau_list_of_term |> encode_args in
  Printf.sprintf "_I%s_%s%s" name return args

(** [translate_primitive p] is the mir representation of primitive [p] *)
let translate_primitive p =
  match p with
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
  | FnCall (id, es) -> translate_fn_call ~ctx id es
  | Length e -> translate_length e
  | Index (e1, e2) -> translate_index e1 e2

(** [translate_arr arr] is the mir representation of [arr] *)
and translate_arr arr =
  let expr_lst = List.map ~f:translate_expr arr in
  translate_arr_lst expr_lst

(** [translate_arr_lst lst] is the mir representation of a list [lst] *)
and translate_arr_lst lst =
  let n = length lst in
  let tm = fresh_temp () in
  let f (len, acc) e =
    let index = len |> to_addr in
    ( Int64.succ len,
      `Move (`Mem (`Bop (`Plus, `Name tm, `Const index)), e) :: acc )
  in
  let add_elts =
    lst |> List.fold ~f ~init:(0L, []) |> snd |> List.rev
  in
  let alloc_arr =
    `Move (tm, `Call (xi_alloc, [ `Const (to_addr n) ]))
  in
  let add_len = `Move (`Mem tm, `Const n) in
  `ESeq
    (`Seq (alloc_arr :: add_len :: add_elts), `Bop (`Plus, tm, eight))

(** [translate_string str] is the mir representation of [str] *)
and translate_string str =
  let f s = `Const (s |> int_of_char |> Int64.of_int) in
  let expr_lst = str |> String.to_list_rev |> List.rev_map ~f in
  translate_arr_lst expr_lst

(** [translate_uop uop e] is the mir representation of unary operator
    expression [uop e] *)
and translate_uop uop e =
  let ir = translate_expr e in
  match uop with
  | `IntNeg -> `Bop (`Plus, log_neg ir, one)
  | `LogNeg -> log_neg ir

(** [translate_bop bop e1 e2] is the mir representation of binary
    operator expression [bop e1 e2] *)
and translate_bop bop e1 e2 =
  match bop with
  | `And ->
      let t, f, label = fresh_label3 () in
      translate_short_circuit e1 e2 label f t f
  | `Or ->
      let t, f, label = fresh_label3 () in
      translate_short_circuit e1 e2 t label t f
  | #Ast.Op.binop ->
      `Bop (Op.coerce bop, translate_expr e1, translate_expr e2)

(** [translate_fn_call ctx id es] is the mir representation of a
    function call with function id [id], arguments [es], and context
    [ctx] *)
and translate_fn_call ~ctx id es =
  let name = mangle id ~ctx in
  let expr_lst = List.map ~f:translate_expr es in
  `Call (`Name name, expr_lst)

(** [translate_length e] is the mir representation of a length function
    call with argument [e] *)
and translate_length e =
  let ir = translate_expr e in
  `Mem (`Bop (`Minus, ir, eight))

(** [translate_index e1 e2] is the mir representation of an indexing of
    [e1] at position [e2] *)
and translate_index e1 e2 =
  let ta, ti = fresh_temp2 () in
  let lok, lerr = fresh_label2 () in
  `ESeq
    ( `Seq
        [
          `Move (ta, translate_expr e1);
          `Move (ti, translate_expr e2);
          `CJump
            ( `Bop (`ULt, ti, `Mem (`Bop (`Minus, `Name ta, eight))),
              lok,
              lerr );
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
  | If (e, s) -> Some (translate_if e s)
  | IfElse (e, s1, s2) -> Some (translate_if_else e s1 s2)
  | While (e, s) -> Some (translate_while e s)
  | VarDecl _ -> None
  | ArrayDecl _ -> None
  | Assign (id, e) -> Some (translate_assign id e)
  | ArrAssign (e1, e2, e3) -> Some (translate_arr_assign e1 e2 e3)
  | ExprStmt (id, es) -> Some (translate_expr_stmt ~ctx id es)
  | VarInit (id, _, e) -> Some (translate_assign id e)
  | MultiAssign (ds, id, es) ->
      Some (translate_multi_assign ~ctx ds id es)
  | PrCall (id, es) -> Some (translate_pr_call ~ctx id es)
  | Return es -> Some (translate_return es)
  | Block stmts -> Some (translate_block stmts)

(** [translate_if_stmt e s] is the first three IR instructions in if and
    if else statements, reversed*)
and translate_if_stmt e s =
  let t_label, f_label = fresh_label2 () in
  [
    translate_stmt s;
    Some (`Label t_label);
    Some (translate_control e t_label f_label);
  ]

(** [translate_if e s] is the mir representation of an if statement with
    condition [e] and body [s] *)
and translate_if e s =
  let f_label = fresh_label () in
  let if_stmt = translate_if_stmt e s in
  let stmts =
    Some (`Label f_label) :: if_stmt |> List.rev |> List.filter_opt
  in
  `Seq stmts

(** [translate_if_else e s1 s2] is the mir representation of an if-else
    statement with condition [e] and statements [s1] and [s2] *)
and translate_if_else e s1 s2 =
  let f_label, end_label = fresh_label2 () in
  let if_stmt = translate_if_stmt e s1 in
  let stmts =
    Some (`Label end_label)
    :: translate_stmt s2
    :: Some (`Label f_label)
    :: Some (`Jump (`Name end_label))
    :: if_stmt
    |> List.rev |> List.filter_opt
  in
  `Seq stmts

(** [translate_while e s] is the mir representation of a while loop with
    condition [e] and loop body [s] *)
and translate_while e s =
  let h_label, t_label, f_label = fresh_label3 () in
  `Seq
    (List.filter_opt
       [
         Some (`Label h_label);
         Some (translate_control e t_label f_label);
         Some (`Label t_label);
         translate_stmt s;
         Some (`Jump (`Name h_label));
         Some (`Label f_label);
       ])

(** [translate_assign id e] is the mir representation of an assignment
    of [e] to [id] *)
and translate_assign id e =
  let expr = translate_expr e in
  `Move (`Temp (PosNode.get id), expr)

(** [translate_arr_assign e1 e2 e3] is the mir representation of a array
    assignment statement with *)
and translate_arr_assign e1 e2 e3 =
  let ta, ti = fresh_temp2 () in
  let lok, lerr = fresh_label2 () in
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
        ( `Mem (`Bop (`Plus, ta, `Bop (`Mul, ti, eight))),
          translate_expr e3 );
    ]

(** [translate_expr_stmt ctx id es] is the mir representation of an
    expression statement with function [id] and arguments [es] *)
and translate_expr_stmt ~ctx id es =
  let name = mangle id ~ctx in
  let expr_lst = List.map ~f:translate_expr es in
  `CallStmt (`Name name, expr_lst)

(** [translate_multi_assign ctx ds id es] is the mir representation of a
    multi-assignment with declarations [ds], and function [id], and
    arguments [es] *)
and translate_multi_assign ~ctx ds id es =
  let name = mangle id ~ctx in
  let expr_lst = List.map ~f:translate_expr es in
  let call = `CallStmt (`Name name, expr_lst) in
  let f (rv, acc) = function
    | None -> (Int.succ rv, acc)
    | Some (v, _) ->
        let t = "_RV" ^ Int.to_string rv in
        let lst = `Move (`Temp (PosNode.get v), `Temp t) :: acc in
        (Int.succ rv, lst)
  in
  let assign = ds |> List.fold ~f ~init:(1, []) |> snd |> List.rev in
  `Seq (call :: assign)

(** [translate_pr_call ctx id es] is the mir representation of a
    procedure call on function [id] with arguments [es] *)
and translate_pr_call ~ctx id es =
  let name = mangle id ~ctx in
  let expr_lst = List.map ~f:translate_expr es in
  `CallStmt (`Name name, expr_lst)

(** [translate_return es] is the mir representation of a return
    statement with expressions [es] *)
and translate_return es = `Return (List.map ~f:translate_expr es)

(** [translate_block stmts] is the mir representation of a statement
    block with statements [stmts] *)
and translate_block stmts =
  `Seq (List.filter_map ~f:translate_stmt stmts)

(** [translate_control enode t f] translates booleans to control flow *)
and translate_control enode t f =
  match DecNode.Expr.get enode with
  | Primitive (`Bool true) -> `Jump (`Name t)
  | Primitive (`Bool false) -> `Jump (`Name f)
  | Uop (`LogNeg, e) -> translate_control e f t
  | Bop (`And, e1, e2) ->
      let label = fresh_label () in
      translate_short_circuit e1 e2 label f t f
  | Bop (`Or, e1, e2) ->
      let label = fresh_label () in
      translate_short_circuit e1 e2 t label t f
  | _ ->
      let expr = translate_expr enode in
      `CJump (expr, t, f)

(** [translate_short_circuit e1 e2 l1 l2 t f] translates the short
    circuit operators [`And] or [`Or] to control flow *)
and translate_short_circuit e1 e2 l1 l2 t f =
  let label = fresh_label () in
  `Seq
    [
      translate_control e1 l1 l2; `Label label; translate_control e2 t f;
    ]

(** [translate_fn_defn sign] is the mir representation of a function
    definition with signature [sign] *)
let translate_fn_defn ({ id } : Ast.signature) block =
  `Seq [ `Label (PosNode.get id); translate_block block ]

(** [translate_global_init id typ p] is the mir representation of a
    global initialization of [id] with type [typ] and primitive [p] as
    its value *)
let translate_global_init id typ p =
  `Move (`Temp (PosNode.get id), translate_primitive p)

(** [translate_defn def] is the mir representation of source toplevel
    definition [def] *)
let translate_defn def =
  match DecNode.Toplevel.get def with
  | FnDefn (signature, block) ->
      Some (translate_fn_defn signature block)
  | GlobalDecl _ -> None
  | GlobalInit (id, typ, p) -> Some (translate_global_init id typ p)

(** [translate_source source] is the mir representation of [source] *)
let translate_source { uses; definitions } =
  `Seq (List.filter_map ~f:translate_defn definitions)

(** [translate_source tnode] is the mir representation of toplevel node
    [tnode] *)
let translate_toplevel tnode =
  match tnode with
  | Source src -> translate_source src
  | Intf intf -> failwith "no IR for an interface"
