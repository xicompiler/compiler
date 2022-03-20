open Core

module type Key = Hashtbl.Key

module type S = sig
  module Key : Key

  type key = Key.t
  type ('v, 'e) vertex
  type ('v, 'e) edge

  module Vertex : sig
    type ('v, 'e) t = ('v, 'e) vertex

    val incoming : ('v, 'e) t -> ('v, 'e) edge list
    val outgoing : ('v, 'e) t -> ('v, 'e) edge list
    val key : ('v, 'e) t -> key
    val value : ('v, 'e) t -> 'v
    val add_edge : src:('v, 'e) t -> dst:('v, 'e) t -> weight:'e -> unit
  end

  module Edge : sig
    type ('v, 'e) t = ('v, 'e) edge

    val src : ('v, 'e) t -> ('v, 'e) vertex
    val dst : ('v, 'e) t -> ('v, 'e) vertex
    val weight : ('v, 'e) t -> 'e
  end

  type ('v, 'e) t

  val create : ?size:int -> unit -> ('v, 'e) t
  val set_vertex : ('v, 'e) t -> key:key -> value:'v -> unit
  val find_vertex : ('v, 'e) t -> key:key -> ('v, 'e) vertex option
  val find_vertex_exn : ('v, 'e) t -> key:key -> ('v, 'e) vertex
end
