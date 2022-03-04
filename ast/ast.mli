(** [Node] is the type of a [Node] in the AST *)
module Node : module type of struct
  include Node.Position
end

include
  Abstract.S with module Expr.Node := Node and module Stmt.Node := Node

val type_check : t -> Decorated.result
(** [type_check ast] is [Ok ast'] where [ast'] is [ast] decorated if
    [ast] represents a semantically valid Xi program, or
    [Error type_error] where [type_error] describes the type error,
    otherwise. *)