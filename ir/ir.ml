open Ast.Op
module Mir = Mir
module Lir = Lir

let ir_expr_of_enode e = failwith "unimplemented"

let ir_expr_of_uop uop e =
  let ir = ir_expr_of_enode e in
  match uop with
  | IntNeg -> `IR_ADD (`IR_NOT ir, 1)
  | LogicalNeg -> `IR_AND (`IR_NOT ir, 1)

let ir_expr_of_bop bop e1 e2 =
  let ir1 = ir_expr_of_enode e1 in
  let ir2 = ir_expr_of_enode e2 in
  match bop with
  | Mult -> `IR_MUL (ir1, ir2)
  | HighMult -> `IR_ARSHIFT (`IR_MUL (ir1, ir2), 32)
  | Div -> `IR_DIV (ir1, ir2)
  | Mod -> `IR_MOD (ir1, ir2)
  | Plus -> `IR_ADD (ir1, ir2)
  | Minus -> `IR_SUB (ir1, ir2)
  | Lt -> `IR_LT (ir1, ir2)
  | Leq -> `IR_LEQ (ir1, ir2)
  | Geq -> `IR_GEQ (ir1, ir2)
  | Gt -> `IR_GT (ir1, ir2)
  | Eq -> `IR_EQ (ir1, ir2)
  | Neq -> `IR_NEQ (ir1, ir2)
  | And -> `IR_AND (ir1, ir2)
  | Or -> `IR_OR (ir1, ir2)
