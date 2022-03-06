open Core

(** [Lazy] contains functions from [Result] that take a thunk as
    argument *)
module Lazy : sig
  val ok_if_true : error:(unit -> 'err) -> bool -> (unit, 'err) result
  (** [Lazy.ok_if_true ~error b] is [Ok ()] if [b] is [true] and
      [Error (error ())] if [b] is false *)

  val of_option : error:(unit -> 'err) -> 'a option -> ('a, 'err) result
  (** Same as [Core.Result.of_option] but takes a thunk as opposed to an
      eager value *)
end

val ( >>? ) : ('a, 'b) result -> ('b -> 'c) -> ('a, 'c) result
(** [r >>? f] is [Core.Result.map_error ~f r] *)
