include
  Abstract.S
    with module Expr.Node = Node.Ident
     and module Stmt.Node = Node.Ident

val type_check : t -> Decorated.result
(** [type_check ast] is [Ok ast'] where [ast'] is [ast] decorated if
    [ast] represents a semantically valid Xi program, or
    [Error type_error] where [type_error] describes the type error,
    otherwise. *)
