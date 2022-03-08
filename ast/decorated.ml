open Core
open Context
open Node

include
  Factory.Make (Decorated.Expr) (Decorated.Stmt) (Decorated.Toplevel)

module Error = Type.Error.Positioned

type expr_result = Expr.node Error.result
type stmt_result = Stmt.node Error.result
type nonrec result = t Error.result
