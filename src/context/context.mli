module Error : module type of Error
open Error
open Type
open Type.Error

type t [@@deriving sexp_of]
(** [t] is the type of a static typing context *)

val empty : t
(** [empty] is an empty typing context having return type [`Unit] *)

val with_ret : ret:[< term ] -> t -> t
(** [with_ret ~ret ctx] is [ctx] requiring that any return statement
    return type [ret] *)

val with_beta : beta:bool -> t -> t
(** [with_beta ~beta ctx] is [ctx] with [beta], representing whether the
    context is within a while loop *)

val ret : t -> term
(** [ret ctx] is the return type of the enclosing function in context
    ctx, or [`Unit] if no return type is needed *)

val beta : t -> bool
(** [beta ctx] is [true] if [ctx] is within a while loop and [false]
    otherwise *)

val find : id:id -> t -> bound Positioned.result
(** [find ~id ctx] is [Ok typ] if [id] is bound to [typ] in [ctx], or
    [Error (Unbound ctx)] if [id] is not bound. *)

val find_var : id:id -> t -> tau Positioned.result
(** [find_var ~id ctx] is [Ok tau] if [id] is bound to expressible type
    [tau] in [ctx], [Error ExpectedTau] if [id] is bound to a function
    type, or [Error (Unbound id)] if [id] is not bound. *)

val find_var_exn : id:id -> t -> tau
(** [find_var_exn ~id ctx] is [find_var ~id ctx] or raises an exn. *)

val find_fn : id:id -> t -> (term * term) Positioned.result
(** [find_fn ~id ctx] is [Ok (t1, t2)] if [id] is bound to function with
    input [t1] and output [t2] [ctx], [Error ExpectedFn] if [id] is
    bound to a tau type, or [Error (Unbound id)] if [id] is not bound. *)

val find_fn_exn : id:id -> t -> term * term
(** [find_fn_exn ~id ctx] is [find_fn ~id ctx] or raises an exn. *)

val find_record : id:id -> t -> (string * tau) list Positioned.result
(** [find_record ~id ctx] is [Ok fields] if [id] is bound to function
    with [fields] in [ctx], [Error ExpectedRecord] if [id] is bound to a
    non-record, or [Error (Unbound id)] if [id] is not bound. *)

val find_record_exn : id:id -> t -> (string * tau) list
(** [find_record_exn ~id ctx] is [find_record ~id ctx] or raises an exn. *)

val add_var : id:id -> typ:tau -> t -> t Positioned.result
(** [add ~id ~typ ctx] is [Ok ctx'] where [ctx'] is
    [ctx :: (id, Var typ)] if [id] is unbound in [ctx], or
    [Error (Bound id)] otherwise *)

val add_fn_defn :
  id:id -> arg:[< term ] -> ret:[< term ] -> t -> t Positioned.result
(** [add_fn_defn ~id ~arg ~ret ctx] is [Ok ctx'] where [ctx'] is
    [ctx :: (id, Fn (arg, ret))] if [id] is unbound in [ctx], or
    [Error (Bound id)] otherwise *)

val add_fn_decl :
  id:id -> arg:[< term ] -> ret:[< term ] -> t -> t Positioned.result
(** [add_fn_decl ~id ~arg ~ret ctx] is [Ok ctx'] where [ctx'] is
    [ctx :: (id, Fn (arg, ret))] if [id] is unbound in [ctx], or
    [Error (Bound id)] otherwise *)

val add_record_defn :
  id:id -> fields:(string * tau) list -> t -> t Positioned.result
(** [add_record_defn ~id ~arg ~ret ctx] is [Ok ctx'] where [ctx'] is
    [ctx :: (id, Record fields)] if [id] is unbound in [ctx], or
    [Error (Bound id)] otherwise *)

val add_record_decl :
  id:id -> fields:(string * tau) list -> t -> t Positioned.result
(** [add_record_decl ~id ~fields ctx] is [Ok ctx'] where [ctx'] is
    [ctx :: (id, Record fields)] if [id] is unbound in [ctx], or
    [Error (Bound id)] otherwise *)
