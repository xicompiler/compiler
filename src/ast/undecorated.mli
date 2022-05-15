open Generic

module Data : sig
  type expr = Position.t
  type stmt = Position.t
  type toplevel = Position.t
end

include Types.S with module Data := Data

type expr = Position.t Generic.Expr.t
(** [expr] is the type of an undecorated expression *)

type stmt = (Position.t, Position.t) Generic.Stmt.t
(** [stmt] is the type of an undecorated statement *)

type find_intf = string -> Position.t Toplevel.intf option
(** A term of type [find_intf] represents a partial mapping between
    interface names and interfaces *)

val type_check :
  file:string -> ?find_intf:find_intf -> t -> Decorated.result
(** [type_check file ?find_intf ast] is [Ok ast'] where [ast'] is [ast]
    decorated if [ast] represents a semantically valid Xi program, or
    [Error type_error] where [type_error] describes the type error,
    otherwise. References to intfs are resolved using [find_intf], which
    returns [None] on any argument by default. *)
