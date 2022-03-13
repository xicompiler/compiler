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

let make_fresh_label () =
  incr label_counter;
  "l" ^ Int.to_string !label_counter

let ir_expr_of_primitive p =
  match p with
  | Int i -> `Const i
  | Bool b -> if b then `Const 1L else `Const 0L
  | Char c -> failwith "unimplemented"

let ir_expr_of_id id = `Temp id

let rec ir_expr_of_enode enode =
  match PosNode.get enode with
  | Primitive p -> ir_expr_of_primitive p
  | Id id -> ir_expr_of_id id
  | Array arr -> failwith "unimplemented"
  | String s -> failwith "unimplemented"
  | Bop (op, e1, e2) -> ir_expr_of_bop op e1 e2
  | Uop (op, e) -> ir_expr_of_uop op e
  | FnCall (id, es) -> ir_expr_of_fncall id es
  | Length node -> failwith "unimplemented"
  | Index (e1, e2) -> failwith "unimplemented"

(* TODO: fix ops *)
and ir_expr_of_uop uop e =
  let ir = ir_expr_of_enode e in
  match uop with
  | IntNeg -> `Bop (`IR_SUB, `CONST 0L, ir)
  | LogicalNeg -> `Bop (`IR_XOR, ir, `CONST 1L)

and ir_expr_of_bop bop e1 e2 =
  let ir1 = ir_expr_of_enode e1 in
  let ir2 = ir_expr_of_enode e2 in
  match bop with
  | Mult -> `Bop (`IR_MUL, ir1, ir2)
  | HighMult -> `Bop (`IR_ARSHIFT, `Bop (`IR_MUL, ir1, ir2), `CONST 32L)
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
  | And -> `Bop (`IR_AND, ir1, ir2)
  | Or -> `Bop (`IR_OR, ir1, ir2)

and ir_expr_of_fncall id es =
  let expr_lst = List.map ir_expr_of_enode es in
  `Call (`Name (PosNode.get id), expr_lst)

let rec ir_stmt_of_snode snode =
  match PosNode.get snode with
  | If (e, s) -> ir_stmt_of_if e s
  | IfElse (e, s1, s2) -> failwith "unimplemented"
  | While (e, s) -> ir_stmt_of_while e s
  | VarDecl (id, typ) -> failwith "unimplemented"
  | ArrayDecl (id, typ, es) -> failwith "unimplemented"
  | Assign (id, e) -> failwith "unimplemented"
  | ArrAssign (e1, e2, e3) -> failwith "unimplemented"
  | ExprStmt (id, es) -> failwith "unimplemented"
  | VarInit (id, typ, e) -> failwith "unimplemented"
  | MultiAssign (ds, id, es) -> failwith "unimplemented"
  | PrCall (id, es) -> failwith "unimplemented"
  | Return es -> failwith "unimplemented"
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

and ir_stmt_of_block stmts = `Seq (List.map ir_stmt_of_snode stmts)
