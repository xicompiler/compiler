open Core

type ('a, 'b) t = ('a, 'b) result
(** [('a, 'b) t] is an alias for [('a, 'b)] result *)

(** [Lazy] contains functions from [Result] that take a thunk as
    argument *)
module Lazy : sig
  val ok_if_true : error:(unit -> 'err) -> bool -> (unit, 'err) t
  (** [Lazy.ok_if_true ~error b] is [Ok ()] if [b] is [true] and
      [Error (error ())] if [b] is false *)

  val of_option : error:(unit -> 'err) -> 'a option -> ('a, 'err) t
  (** Same as [Core.Result.of_option] but takes a thunk as opposed to an
      eager value *)
end

val join_error : ('a, ('a, 'err) t) t -> ('a, 'err) t
(** [join_error rr] is [e] if [r] is [Error e] and [r] otherwise *)

val compose_ok : ('a -> 'b) -> 'a -> ('b, 'err) t
(** [compose_ok f x] is [Ok (f x)]*)

val ok : ('a, 'b) result -> 'a
(** [ok res] is [v] if [res] is [Ok v] or raises an exn *)

val ( >>? ) : ('a, 'b) result -> ('b -> 'c) -> ('a, 'c) result
(** [r >>? f] is [Core.Result.map_error ~f r] *)
