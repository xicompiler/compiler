open Core

module type Key = Graph.Key
(** [Key] is the module type of a key in the graph *)

(** [S] is the abstract type of a graph *)
module type S = sig
  module Key : Key
  (** [Key] represents the key in the graph *)

  type ('v, 'e) vertex
  (** A [('v, 'e) vertex] is a vertex carrying a value of type ['v] and
      adjacent to edges carrying type ['e] *)

  type ('v, 'e) edge
  (** A [('v, 'e) edge] is an edge carrying a value of type ['e] and
      incident to vertexs carrying type ['v] *)

  (** [Vertex] represents a vertex in a directed graph *)
  module Vertex : sig
    include
      Graph.Vertex.S2
        with type ('v, 'e) t = ('v, 'e) vertex
        with module Key := Key

    val incoming : ('v, 'e) t -> ('v, 'e) edge list
    (** [incoming vertex] is the list of edges entering [vertex] *)

    val pred : ('v, 'e) t -> ('v, 'e) t list
    (** [pred vertex] is the list of predecessor nodes of [vertex] *)

    val outgoing : ('v, 'e) t -> ('v, 'e) edge list
    (** [outgoing vertex] is the list of exiting [vertex] *)

    val succ : ('v, 'e) t -> ('v, 'e) t list
    (** [succ vertex] is the list of successor nodes of [vertex] *)

    val has_succ : ('v, 'e) t -> target:('v, 'e) t -> bool
    (** [has_succ v ~target] is [true] iff the successors of [v] include
        [target] by physical equality *)

    val peek_succ : ('v, 'e) t -> ('v, 'e) t option
    (** [peek_succ u] is [Some v] if there exists an edge [(u, v)], or
        [None] if [u] has no successors *)

    val has_unmarked_pred : ('v, 'e) t -> bool
    (** [has_marked_pred vertex] is [true] iff [vertex] has an unmarked
        predecessor *)

    val add_edge : src:('v, 'e) t -> dst:('v, 'e) t -> weight:'e -> unit
    (** [add_edge ~src ~dst ~weight] adds an edge [(src, dst)] with
        weight [weight] *)

    val set_edge : src:('v, 'e) t -> dst:('v, 'e) t -> weight:'e -> unit
    (** [set_edge ~src ~dst ~weight] removes any edge [(src, dst)] and
        adds a fresh one with weight [weight ]*)

    val remove_edge : src:('v, 'e) t -> dst:('v, 'e) t -> unit
    (** [remove_edge ~src ~dst] removes all edges [(src, dst)] *)

    val exists_incoming :
      ('v, 'e) t -> f:(('v, 'e) edge -> bool) -> bool
    (** [exists_incoming v ~f] is [true] iff there exists an edge [e]
        entering [v] for which [f e] is [true] *)
  end

  (** [Edge] represents an edge in a directed graph *)
  module Edge : sig
    type ('v, 'e) t = ('v, 'e) edge
    (** A [('v, 'e) t] is an edge carrying a value of type ['e] and
        incident to vertexs carrying type ['v] *)

    val src : ('v, 'e) t -> ('v, 'e) vertex
    (** [src edge] is the source vertex of [edge] *)

    val dst : ('v, 'e) t -> ('v, 'e) vertex
    (** [dst edge] is the destination vertex of [edge] *)

    val weight : ('v, 'e) t -> 'e
    (** [weight edge] is the weight carried by [edge] *)
  end

  type ('v, 'e) t
  (** [('v, 'e) t] is the type of a graph with vertices carrying values
      of type ['v] and edges carrying values of type ['e] *)

  val create : ?size:int -> unit -> ('v, 'e) t
  (** [create ~size ()] is a fresh graph with size [size] *)

  (** [Dataflow] contains operations for performing dataflow analysis *)
  module Dataflow : sig
    type 'data values = {
      input : 'data;
      output : 'data;
    }
    (** ['data values] represent the dataflow values computed at each
        node, i.e. the dataflow values that are the input and output to
        the transfer function *)

    type 'data map = Key.t -> 'data values
    (** A ['data map] maps a vertex's unique key to its data *)

    val analyze :
      ('v, 'e) t -> ('data, 'v) Dataflow.Params.t -> 'data map
    (** [analyze g params] is a function [data : key -> 'data] such
        that, for any bound node key [k], [data k] is the dataflow value
        associated with the node with key [k] *)
  end

  val graphviz :
    string_of_vertex:(('v, 'e) vertex -> string) ->
    string_of_weight:('e -> string) ->
    ('v, 'e) vertex list ->
    string
end
