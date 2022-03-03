open Core
open Definitions
open TypeError
include Map.S with type Key.t = string

type context = id t
(** [context] is the type of a static typing context *)

val find : id:string -> context -> id result
(** [find ~id ctx] is [Ok typ] if [id] is bound in [ctx], or
    [Error (Unbound ctx)] if [id] is not bound. *)

(** [Fn] represents a typing context present within a function body *)
module Fn : sig
  type t
  (** [t] represents the typing context found within a function. *)

  val find : id:string -> t -> id result
  (** [find ~id ctx] is [Ok typ] if [id] is bound in [ctx], or
      [Error (Unbound ctx)] if [id] is not bound. *)

  val add : id:string -> typ:id -> t -> t result
  (** [add ~id ~typ ctx] is [Ok ctx'] where [ctx'] is [ctx :: (id, typ)]
      if [id] is unbound in [ctx], or [Error (Bound id)] otherwise *)

  val add_var : id:string -> typ:tau -> t -> t result
  (** [add ~id ~typ ctx] is [Ok ctx'] where [ctx'] is
      [ctx :: (id, Var typ)] if [id] is unbound in [ctx], or
      [Error (Bound id)] otherwise *)

  val context : t -> context
  (** [context fn_ctx] is the typing context found within [fn_ctx] *)

  val ret : t -> term
  (** [ret fn_ctx] is the return type of the function represented by
      [fn_ctx] *)
end

type fn = Fn.t
(** [fn] is an alias for [Fn.t] *)