open Core
include Factory.Make (Type.Node.Expr) (Type.Node.Stmt)

module Error = struct
  type t = {
    cause : Type.error;
    pos : Position.t;
  }

  let make ~pos cause = { cause; pos }
  let cause { cause } = cause
  let pos { pos } = pos

  type nonrec 'a result = ('a, t) result
end

type expr_result = Expr.node Error.result
type stmt_result = Stmt.node Error.result
type nonrec result = t Error.result