open Core
include Factory.Make (Type.Node.Expr) (Type.Node.Stmt)

type nonrec result = (t, Type.error Position.error) result
