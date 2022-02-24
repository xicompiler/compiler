open Factory
open Abstract
include Factory.Make (Types.Untyped) (Node.Ident) (Node.Ident)

type nonrec expr = unit expr

let make_bop bop e1 e2 =
  match bop with
  | (`Mult | `HighMult | `Div | `Mod | `Plus | `Minus) as arith ->
      Expr.Arith (arith, e1, e2)
  | (`Lt | `Leq | `Geq | `Gt) as cmp -> Expr.Compare (cmp, e1, e2)
  | (`Eq | `Neq) as eq -> Expr.Equality (eq, e1, e2)
  | (`And | `Or) as log -> Expr.Logical (log, e1, e2)
