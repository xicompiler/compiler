open Core

module type Key = Hashtbl.Key

module type S = sig
  module Key : Key

  type key = Key.t
  type ('v, 'e) vertex
  type ('v, 'e) edge

  module Vertex : sig
    type ('v, 'e) t = ('v, 'e) vertex

    val create : key:key -> value:'v -> ('v, 'e) t
    val incoming : ('v, 'e) t -> ('v, 'e) edge list
    val pred : ('v, 'e) t -> ('v, 'e) t list
    val outgoing : ('v, 'e) t -> ('v, 'e) edge list
    val succ : ('v, 'e) t -> ('v, 'e) t list
    val has_succ : ('v, 'e) t -> target:('v, 'e) t -> bool
    val peek_succ : ('v, 'e) t -> ('v, 'e) t option
    val key : ('v, 'e) t -> key
    val value : ('v, 'e) t -> 'v
    val set : ('v, 'e) t -> value:'v -> unit
    val map_set : ('v, 'e) t -> f:('v -> 'v) -> unit
    val map : ('v, 'e) t -> f:('v -> 'a) -> 'a
    val marked : ('v, 'e) t -> bool
    val unmarked : ('v, 'e) t -> bool
    val mark : ('v, 'e) t -> unit
    val has_unmarked_pred : ('v, 'e) t -> bool
    val add_edge : src:('v, 'e) t -> dst:('v, 'e) t -> weight:'e -> unit
    val set_edge : src:('v, 'e) t -> dst:('v, 'e) t -> weight:'e -> unit
    val remove_edge : src:('v, 'e) t -> dst:('v, 'e) t -> unit

    val exists_incoming :
      ('v, 'e) t -> f:(('v, 'e) edge -> bool) -> bool

    val equal : ('v, 'e) t -> ('v, 'e) t -> bool
  end

  module Edge : sig
    type ('v, 'e) t = ('v, 'e) edge

    val src : ('v, 'e) t -> ('v, 'e) vertex
    val dst : ('v, 'e) t -> ('v, 'e) vertex
    val weight : ('v, 'e) t -> 'e
  end

  val graphviz :
    string_of_vertex:(('v, 'e) vertex -> string) ->
    string_of_weight:('e -> string) ->
    ('v, 'e) vertex list ->
    string
end