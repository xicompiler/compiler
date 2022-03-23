open Core

type 'a t = 'a option
(** ['a t] is an alias for ['a option] *)

(** [Lazy] contains functions operating on thunks *)
module Lazy : sig
  val value : default:(unit -> 'a) -> 'a t -> 'a
  (** [value ~default o] is [x] if [o] is [Some x] and [default ()] if
      [o] is [None] *)
end
