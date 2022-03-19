open Core

(** [Lazy] contains functions operating on thunks *)
module Lazy : sig
  val value : default:(unit -> 'a) -> 'a option -> 'a
  (** [value ~default o] is [x] if [o] is [Some x] and [default ()] if
      [o] is [None] *)
end
