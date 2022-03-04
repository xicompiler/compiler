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

val type_check_expr :
  ctx:Type.context -> Expr.node -> Decorated.expr_result
(** [type_check_expr ~ctx expr] is [Ok expr'] where [expr'] is [expr]
    decorated if [expr] represents a semantically valid Xi expression in
    context [ctx], or [Error type_error] where [type_error] describes
    the type error, otherwise. *)

val type_check_stmt :
  ctx:Type.Context.fn -> Stmt.node -> Decorated.stmt_result
(** [type_check_stmt stmt] is [Ok stmt'] where [stmt'] is [stmt]
    decorated if [stmt] represents a semantically valid Xi expression in
    context [ctx], or [Error type_error] where [type_error] describes
    the type error, otherwise. *)
