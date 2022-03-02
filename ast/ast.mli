include
  Abstract.S
    with module Expr.Node = Node.Pos
     and module Stmt.Node = Node.Pos

val type_check : t -> Decorated.result
(** [type_check ast] is [Ok ast'] where [ast'] is [ast] decorated if
    [ast] represents a semantically valid Xi program, or
    [Error type_error] where [type_error] describes the type error,
    otherwise. *)

module Tau : module type of Tau
(** export [Tau] for use by Menhir *)