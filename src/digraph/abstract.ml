open Core

module type Key = Graph.Key

module type S = sig
  module Key : Key

  type ('v, 'e) vertex
  type ('v, 'e) edge

  module Vertex : sig
    include
      Graph.Vertex.S2
        with type ('v, 'e) t = ('v, 'e) vertex
        with module Key := Key

    val incoming : ('v, 'e) t -> ('v, 'e) edge list
    val pred : ('v, 'e) t -> ('v, 'e) t list
    val outgoing : ('v, 'e) t -> ('v, 'e) edge list
    val succ : ('v, 'e) t -> ('v, 'e) t list
    val has_succ : ('v, 'e) t -> target:('v, 'e) t -> bool
    val peek_succ : ('v, 'e) t -> ('v, 'e) t option
    val has_unmarked_pred : ('v, 'e) t -> bool

    val add_unweighted_edge :
      src:('v, unit) t -> dst:('v, unit) t -> unit

    val add_edge : src:('v, 'e) t -> dst:('v, 'e) t -> weight:'e -> unit
    val set_edge : src:('v, 'e) t -> dst:('v, 'e) t -> weight:'e -> unit
    val remove_edge : src:('v, 'e) t -> dst:('v, 'e) t -> unit

    val exists_incoming :
      ('v, 'e) t -> f:(('v, 'e) edge -> bool) -> bool
  end

  module Edge : sig
    type ('v, 'e) t = ('v, 'e) edge

    val src : ('v, 'e) t -> ('v, 'e) vertex
    val dst : ('v, 'e) t -> ('v, 'e) vertex
    val weight : ('v, 'e) t -> 'e
  end

  type ('v, 'e) t

  val create : ?size:int -> unit -> ('v, 'e) t
  val iter_vertices : ('v, 'e) t -> f:(('v, 'e) vertex -> unit) -> unit
  val add_vertex : ('v, 'e) t -> ('v, 'e) vertex -> unit
  val of_vertices : ('v, 'e) vertex list -> ('v, 'e) t

  module Dataflow : sig
    type 'data values = {
      input : 'data;
      output : 'data;
    }

    type 'data map = Key.t -> 'data values

    val analyze :
      ('v, 'e) t -> ('data, 'v) Dataflow.Params.t -> 'data map
  end

  val graphviz :
    string_of_vertex:(('v, 'e) vertex -> string) ->
    string_of_weight:('e -> string) ->
    ('v, 'e) vertex list ->
    string
end
