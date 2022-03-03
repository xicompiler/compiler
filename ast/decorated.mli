open Core

include
  Abstract.S
    with module Expr.Node := Type.Node.Expr
     and module Stmt.Node := Type.Node.Stmt

type nonrec result = (t, Type.error Position.error) result
(** [result] is either [Ok ast] or [Error err], where [err] describes
    the position and cause of the semantic error. *)
