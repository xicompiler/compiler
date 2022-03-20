open Core
open Ast.Op
module Mir = Mir
module Lir = Lir
module PosNode = Node.Position
open Ast.Expr
open Ast.Stmt
open Ast.Toplevel
open Ast.Primitive
open Type

let label_counter = ref 0

let temp_counter = ref 0

let make_fresh pre counter =
  incr counter;
  pre ^ Int.to_string !counter

(** [make_fresh_label ()] makes a fresh (unused elsewhere) label with
    prefix "l" and integer based on [label_counter] *)
let make_fresh_label () = make_fresh "l" label_counter

(** [make_fresh_temp ()] makes a fresh (unused elsewhere) temp name with
    prefix "x" and integer based on [temp_counter] *)
let make_fresh_temp () = make_fresh "x" temp_counter

(* TODO: mangling function names *)
let mangle fn = failwith "unimplemented"

let ir_expr_of_primitive p =
  match p with
  | Int i -> `Const i
  | Bool b -> if b then `Const 1L else `Const 0L
  | Char c -> `Const (c |> Uchar.to_scalar |> Int64.of_int)

let ir_expr_of_id id = `Temp id

let rec ir_expr_of_enode enode =
  match PosNode.get enode with
  | Primitive p -> ir_expr_of_primitive p
  | Id id -> ir_expr_of_id (PosNode.get id)
  | Array arr -> ir_expr_of_arr arr
  | String s -> ir_expr_of_string s
  | Bop (op, e1, e2) -> ir_expr_of_bop op e1 e2
  | Uop (op, e) -> ir_expr_of_uop op e
  | FnCall (id, es) -> ir_expr_of_fncall id es
  | Length node -> ir_expr_of_length node
  | Index (e1, e2) -> ir_expr_of_index e1 e2

and ir_expr_of_arr arr =
  let e_nodes = List.map ~f:ir_expr_of_enode (Array.to_list arr) in
  ir_expr_of_arr_lst e_nodes

and ir_expr_of_arr_lst lst =
  let n = Int64.of_int (List.length lst) in
  let tm = make_fresh_temp () in
  let f acc e =
    let index =
      acc |> List.length |> Int64.of_int |> Int64.( * ) 8L
      |> Int64.( + ) 8L
    in
    `Move (`Mem (`Bop (`IR_ADD, `Temp tm, `Const index)), e) :: acc
  in
  let add_elts = List.rev (List.fold_left ~f ~init:[] lst) in
  `ESeq
    ( `Seq
        (`Move
           ( `Temp tm,
             `Call
               ( `Name "_xi_alloc",
                 [ `Const (n |> Int64.( * ) 8L |> Int64.( + ) 8L) ] ) )
        :: `Move (`Mem (`Temp tm), `Const n)
        :: add_elts),
      `Bop (`IR_ADD, `Temp tm, `Const 8L) )

and ir_expr_of_string str =
  let arr =
    List.map
      ~f:(fun s -> `Const (s |> int_of_char |> Int64.of_int))
      (String.to_list str)
  in
  ir_expr_of_arr_lst arr

(* TODO: fix ops *)
and ir_expr_of_uop uop e =
  let ir = ir_expr_of_enode e in
  match uop with
  | IntNeg -> `Bop (`IR_SUB, `Const 0L, ir)
  | LogicalNeg -> `Bop (`IR_XOR, ir, `Const 1L)

and ir_expr_of_bop bop e1 e2 =
  let ir1 = ir_expr_of_enode e1 in
  let ir2 = ir_expr_of_enode e2 in
  match bop with
  | Mult -> `Bop (`IR_MUL, ir1, ir2)
  | HighMult -> `Bop (`IR_ARSHIFT, `Bop (`IR_MUL, ir1, ir2), `Const 32L)
  | Div -> `Bop (`IR_DIV, ir1, ir2)
  | Mod -> `Bop (`IR_MOD, ir1, ir2)
  | Plus -> `Bop (`IR_ADD, ir1, ir2)
  | Minus -> `Bop (`IR_SUB, ir1, ir2)
  | Lt -> `Bop (`IR_LT, ir1, ir2)
  | Leq -> `Bop (`IR_LEQ, ir1, ir2)
  | Geq -> `Bop (`IR_GEQ, ir1, ir2)
  | Gt -> `Bop (`IR_GT, ir1, ir2)
  | Eq -> `Bop (`IR_EQ, ir1, ir2)
  | Neq -> `Bop (`IR_NEQ, ir1, ir2)
  | And -> ir_expr_of_and ir1 ir2
  | Or -> ir_expr_of_or ir1 ir2

and ir_expr_of_and ir1 ir2 =
  let x = make_fresh_temp () in
  let l1 = make_fresh_label () in
  let l2 = make_fresh_label () in
  let lf = make_fresh_label () in
  `ESeq
    ( `Seq
        [
          `Move (`Temp x, `Const 0L);
          `CJump (ir1, `Label l1, `Label lf);
          `Label l1;
          `CJump (ir2, `Label l2, `Label lf);
          `Label l2;
          `Move (`Temp x, `Const 1L);
          `Label lf;
        ],
      `Temp x )

and ir_expr_of_or ir1 ir2 =
  let x = make_fresh_temp () in
  let l1 = make_fresh_label () in
  let l2 = make_fresh_label () in
  let lt = make_fresh_label () in
  `ESeq
    ( `Seq
        [
          `Move (`Temp x, `Const 1L);
          `CJump (ir1, `Label lt, `Label l1);
          `Label l1;
          `CJump (ir2, `Label lt, `Label l2);
          `Label l2;
          `Move (`Temp x, `Const 0L);
          `Label lt;
        ],
      `Temp x )

and ir_expr_of_fncall id es =
  let expr_lst = List.map ~f:ir_expr_of_enode es in
  `Call (`Name (PosNode.get id), expr_lst)

and ir_expr_of_length node =
  let e = ir_expr_of_enode node in
  `Mem (`Bop (`IR_SUB, e, `Const 8L))

and ir_expr_of_index e1 e2 =
  let expr1 = ir_expr_of_enode e1 in
  let expr2 = ir_expr_of_enode e2 in
  let ta = make_fresh_temp () in
  let ti = make_fresh_temp () in
  let lok = make_fresh_label () in
  let lerr = make_fresh_label () in
  `ESeq
    ( `Seq
        [
          `Move (`Temp ta, expr1);
          `Move (`Temp ti, expr2);
          `CJump
            ( `Bop
                ( `IR_ULT,
                  `Temp ti,
                  `Mem (`Bop (`IR_SUB, `Temp ta, `Const 8L)) ),
              `Label lok,
              `Label lerr );
          `Label lerr;
          `Call (`Name "_xi_out_of_bounds", []);
          `Label lok;
        ],
      `Mem (`Bop (`IR_ADD, ta, `Bop (`IR_MUL, ti, `Const 8L))) )

and ir_stmt_of_snode snode =
  match PosNode.get snode with
  | If (e, s) -> ir_stmt_of_if e s
  | IfElse (e, s1, s2) -> failwith "unimplemented"
  | While (e, s) -> ir_stmt_of_while e s
  | VarDecl (id, typ) -> failwith "unimplemented"
  | ArrayDecl (id, typ, es) -> ir_stmt_of_array_decl id typ es
  | Assign (id, e) -> ir_stmt_of_assign id e
  | ArrAssign (e1, e2, e3) -> failwith "unimplemented"
  | ExprStmt (id, es) -> failwith "unimplemented"
  | VarInit (id, typ, e) -> failwith "unimplemented"
  | MultiAssign (ds, id, es) -> ir_stmt_of_multi_assign ds id es
  | PrCall (id, es) -> ir_stmt_of_prcall id es
  | Return es -> ir_stmt_of_return es
  | Block stmts -> ir_stmt_of_block stmts

and ir_stmt_of_if e s =
  let expr = ir_expr_of_enode e in
  let stmt = ir_stmt_of_snode s in
  let t_label = make_fresh_label () in
  let f_label = make_fresh_label () in
  `Seq
    [
      `CJump (expr, `Label t_label, `Label f_label);
      `Label t_label;
      stmt;
      `Label f_label;
    ]

and ir_stmt_of_while e s =
  let expr = ir_expr_of_enode e in
  let stmt = ir_stmt_of_snode s in
  let h_label = make_fresh_label () in
  let t_label = make_fresh_label () in
  let f_label = make_fresh_label () in
  `Seq
    [
      `Label h_label;
      `CJump (expr, `Label t_label, `Label f_label);
      `Label t_label;
      stmt;
      `Jump (`Name h_label);
      `Label f_label;
    ]

and ir_stmt_of_array_decl id typ es = failwith "unimplemented"

and ir_stmt_of_assign id e =
  let expr = ir_expr_of_enode e in
  `Move (`Temp id, expr)

and ir_stmt_of_multi_assign ds id es = failwith "unimplemented"

and ir_stmt_of_prcall id es =
  let expr_lst = List.map ~f:ir_expr_of_enode es in
  `Call (`Name (PosNode.get id), expr_lst)

and ir_stmt_of_return es = `Return (List.map ~f:ir_expr_of_enode es)

and ir_stmt_of_block stmts = `Seq (List.map ~f:ir_stmt_of_snode stmts)

and ir_of_fn_defn signature block = failwith "unimplemented"
(*`Seq [ `Label signature.id; ir_stmt_of_block block ]*)

and ir_stmt_of_control enode t f = 
  match PosNode.get enode with
  | Primitive (Bool true) -> `Jump (`Name t)
  | Primitive (Bool false) -> `Jump (`Name f)
  | Uop (LogicalNeg, e) -> ir_stmt_of_control e f t
  | Bop (And, e1, e2) -> let label = make_fresh_label () in
    `Seq
      [
        ir_stmt_of_control e1 label f;
        `Label label;
        ir_stmt_of_control e2 t f;
      ]
  | Bop (Or, e1, e2) -> let label = make_fresh_label () in
    `Seq
      [
        ir_stmt_of_control e1 t label;
        `Label label;
        ir_stmt_of_control e2 t f;
      ]
  | _ -> let expr = ir_expr_of_enode enode in
    `CJump (expr, t, f)
