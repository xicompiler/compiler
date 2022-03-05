open Core
open Definitions
open TypeError

type t
(** [t] is the type of a static typing context *)

val empty : t
(** [empty] is an empty typing context having return type [`Unit] *)

val with_ret : ret:term -> t -> t
(** [with_ret ~ret ctx] is [ctx] requiring that any return statement
    return type [ret] *)

val ret : t -> term
(** [ret ctx] is the return type of the enclosing function in context
    ctx, or [`Unit] if no return type is needed *)

val find : id:string -> t -> id result
(** [find ~id ctx] is [Ok typ] if [id] is bound to [typ] in [ctx], or
    [Error (Unbound ctx)] if [id] is not bound. *)

val find_var : id:string -> t -> tau result
(** [find_var ~id ctx] is [Ok tau] if [id] is bound to expressible type
    [tau] in [ctx], [Error ExpectedTau] if [id] is bound to a function
    type, or [Error (Unbound id)] if [id] is not bound. *)

val find_fn : id:string -> t -> (term * term) result
(** [find_fn ~id ctx] is [Ok (t1, t2)] if [id] is bound to function with
    input [t1] and output [t2] [ctx], [Error ExpectedFn] if [id] is
    bound to a tau type, or [Error (Unbound id)] if [id] is not bound. *)

val add : id:string -> typ:id -> t -> t result
(** [add ~id ~typ ctx] is [Ok ctx'] where [ctx'] is [ctx :: (id, typ)]
    if [id] is unbound in [ctx], or [Error (Bound id)] otherwise *)

val add_var : id:string -> typ:tau -> t -> t result
(** [add ~id ~typ ctx] is [Ok ctx'] where [ctx'] is
    [ctx :: (id, Var typ)] if [id] is unbound in [ctx], or
    [Error (Bound id)] otherwise *)

val add_fn : id:string -> arg:term -> ret:term -> t -> t result
(** [add ~id ~arg ~ret ctx] is [Ok ctx'] where [ctx'] is
    [ctx :: (id, Fn (arg, ret))] if [id] is unbound in [ctx], or
    [Error (Bound id)] otherwise *)

val add_fn_list :
  id:string -> arg:tau list -> ret:tau list -> t -> t result
(** Same as [add_fn] but takes list of argument and return types *)
