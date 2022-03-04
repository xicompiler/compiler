(** A [unop] is the type of a Xi unary operator *)
type unop =
  | IntNeg
  | LogicalNeg

(** A [binop] is the type of a Xi binary operator *)
type binop =
  | Mult
  | HighMult
  | Div
  | Mod
  | Plus
  | Minus
  | Lt
  | Leq
  | Geq
  | Gt
  | Eq
  | Neq
  | And
  | Or
