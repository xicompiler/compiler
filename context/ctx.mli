open Core
open CtxError
open Type
open Type.Error

type t [@@deriving sexp_of]
(** [t] is the type of a static typing context *)

val empty : t
(** [empty] is an empty typing context having return type [`Unit] *)

val io : t
(** [io] is a context corresponding to the [io] module in Xi *)

val conv : t
(** [conv] is a context corresponding to the [conv] module in Xi *)

val with_ret : ret:[< term ] -> t -> t
(** [with_ret ~ret ctx] is [ctx] requiring that any return statement
    return type [ret] *)

val ret : t -> term
(** [ret ctx] is the return type of the enclosing function in context
    ctx, or [`Unit] if no return type is needed *)

val find : id:id -> t -> bound Positioned.result
(** [find ~id ctx] is [Ok typ] if [id] is bound to [typ] in [ctx], or
    [Error (Unbound ctx)] if [id] is not bound. *)

val find_var : id:id -> t -> tau Positioned.result
(** [find_var ~id ctx] is [Ok tau] if [id] is bound to expressible type
    [tau] in [ctx], [Error ExpectedTau] if [id] is bound to a function
    type, or [Error (Unbound id)] if [id] is not bound. *)

val find_fn : id:id -> t -> (term * term) Positioned.result
(** [find_fn ~id ctx] is [Ok (t1, t2)] if [id] is bound to function with
    input [t1] and output [t2] [ctx], [Error ExpectedFn] if [id] is
    bound to a tau type, or [Error (Unbound id)] if [id] is not bound. *)

val add_var : id:id -> typ:tau -> t -> t Positioned.result
(** [add ~id ~typ ctx] is [Ok ctx'] where [ctx'] is
    [ctx :: (id, Var typ)] if [id] is unbound in [ctx], or
    [Error (Bound id)] otherwise *)

val add_fn_defn :
  id:id -> arg:[< term ] -> ret:[< term ] -> t -> t Positioned.result
(** [add_fn_defn ~id ~arg ~ret ctx] is [Ok ctx'] where [ctx'] is
    [ctx :: (id, Fn (arg, ret))] if [id] is unbound in [ctx], or
    [Error (Bound id)] otherwise *)

val add_fn_defn_list :
  id:id -> arg:tau list -> ret:tau list -> t -> t Positioned.result
(** Same as [add_fn_defn] but takes list of argument and return types *)

val add_fn_decl :
  id:id -> arg:[< term ] -> ret:[< term ] -> t -> t Positioned.result
(** [add_fn_decl ~id ~arg ~ret ctx] is [Ok ctx'] where [ctx'] is
    [ctx :: (id, Fn (arg, ret))] if [id] is unbound in [ctx], or
    [Error (Bound id)] otherwise *)

val add_fn_decl_list :
  id:id -> arg:tau list -> ret:tau list -> t -> t Positioned.result
(** Same as [add_fn_decl] but takes list of argument and return types *)
