include module type of Abstract

include
  S
    with module Expr.Node := Node.Position
     and module Stmt.Node := Node.Position
     and module Toplevel.Node := Node.Position

val type_check :
  ?find_intf:(string -> Toplevel.intf option) -> t -> Decorated.result
(** [type_check ast] is [Ok ast'] where [ast'] is [ast] decorated if
    [ast] represents a semantically valid Xi program, or
    [Error type_error] where [type_error] describes the type error,
    otherwise. References to intfs are resolved using [find_intf], which
    returns [None] on any argument by default. *)

val type_check_expr :
  ctx:Context.t -> Expr.node -> Decorated.expr_result
(** [type_check_expr ~ctx expr] is [Ok expr'] where [expr'] is [expr]
    decorated if [expr] represents a semantically valid Xi expression in
    context [ctx], or [Error type_error] where [type_error] describes
    the type error, otherwise. *)

val type_check_stmt :
  ctx:Context.t -> Stmt.node -> Decorated.stmt_result
(** [type_check_stmt stmt] is [Ok stmt'] where [stmt'] is [stmt]
    decorated if [stmt] represents a semantically valid Xi expression in
    context [ctx], or [Error type_error] where [type_error] describes
    the type error, otherwise. *)

module Decorated : module type of struct
  include Decorated
end
