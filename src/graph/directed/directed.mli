open! Core

include module type of struct
  include Abstract
end

include module type of struct
  include Factory
end

module IntDigraph : S with type Key.t = int
(** An [IntDigraph] is an [S] where vertices are indexed using unique
    integers *)
