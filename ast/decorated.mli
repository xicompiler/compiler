open Core

include
  Abstract.S
    with module Expr.Node := Type.Node.Expr
     and module Stmt.Node := Type.Node.Stmt

(** [Error] represents a semantic error in the AST *)
module Error : sig
  type t
  (** [t] is the abstract type of an error *)

  val make : pos:Position.t -> Type.error -> t
  (** [make ~pos cause] is a semantic error occurring at pos [pos]
      having semantic cause [cause] *)

  val cause : t -> Type.error
  (** [cause error] is the cause of [error] *)

  val pos : t -> Position.t
  (** [pos error] is the position at which [error] occurs *)

  type nonrec 'a result = ('a, t) result
  (** An ['a result] is either [Ok 'a] or a semantic error *)
end

type expr_result = Expr.node Error.result
(** An [expr_result] is either [Ok expr] or [Error type_error] *)

type stmt_result = Stmt.node Error.result
(** An [stmt_result] is either [Ok stmt] or [Error type_error] *)

type nonrec result = t Error.result
(** A [result] is either [Ok ast] where [ast] is a decorated AST or
    [Error err] where [err] details the semantic error causing
    decoration to fail *)
