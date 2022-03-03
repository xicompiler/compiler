open Definitions
open TypeError

module type S = sig
  include Node.S

  type typ
  (** [typ] is the type of a value wrapped in a node *)

  type context
  (** [context] is the type of the context wrapped in a node *)

  type nonrec 'a result = ('a, error Position.error) Result.t
  (** An ['a result] is either [Ok 'a] or [Error err], where [err]
      describes the cause and type of a semantic error. *)

  val context : 'a t -> context
  (** [context node] is the context of node *)

  val typ : 'a t -> typ
  (** [typ v] is the type of the value wrapped in [v] *)

  val make : 'a -> ctx:context -> typ:typ -> 'a t
  (** [make v ~ctx ~typ] is a node wrapping value [v] with context [ctx]
      and type [typ] *)
end

(** [Expr] is a module wrapping an expression node *)
module Expr : S with type typ = expr and type context = Context.context

(** [Stmt] is a module wrapping a statement node *)
module Stmt : sig
  include S with type typ = stmt and type context = Context.fn

  val make_unit : 'a -> ctx:context -> 'a t
  (** [make_unit v ~ctx] is [make v ~ctx ~typ:`Unit] *)
end
