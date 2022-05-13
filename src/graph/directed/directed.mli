open! Core

include module type of struct
  include Abstract
end

include module type of struct
  include Factory
end

(** An [IntDigraph] is an [S] where vertices are indexed using unique
    integers *)
module IntDigraph : sig
  include S with type Key.t = int

  val unused_key : ('v, 'e) t -> Key.t
  (** No vertex in [g] has a key equal to [unused_key g] *)
end
