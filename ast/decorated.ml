open Core
include Factory.Make (Type.Node.Expr) (Type.Node.Stmt) (Node.Position)
module Error = Type.Error.Positioned

type expr_result = Expr.node Error.result
type stmt_result = Stmt.node Error.result
type nonrec result = t Error.result