open Core

type ('v, 'e) vertex
(** A [('v, 'e) vertex] is a vertex carrying a value of type ['v] and
    adjacent to edges carrying type ['e] *)

type ('v, 'e) edge
(** A [('v, 'e) edge] is an edge carrying a value of type ['e] and
    incident to vertexs carrying type ['v] *)

(** [Vertex] represents a vertex in a directed graph *)
module Vertex : sig
  type ('v, 'e) t = ('v, 'e) vertex
  (** A [('v, 'e) t] is a vertex carrying a value of type ['v] and
      adjacent to edges carrying type ['e] *)

  val create : 'v -> ('v, 'e) t
  (** [create value] is a fresh unmarked vertex with value [value] and
      no incident edges *)

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

  val value : ('v, 'e) t -> 'v
  (** [value vertex] is the value stored by [vertex] *)

  val set : ('v, 'e) t -> value:'v -> unit
  (** [set_value v ~value] sets the value of vertex [v] to [value] *)

  val map_set : ('v, 'e) t -> f:('v -> 'v) -> unit
  (** [map_set v ~f] applies [f] to the value of [g] and writes the
      computed result back to [v] *)

  val map : ('v, 'e) t -> f:('v -> 'a) -> 'a
  (** [map vertex ~f] is [f (value vertex)] *)

  val marked : ('v, 'e) t -> bool
  (** [marked vertex] is [true] iff [vertex] has been marked *)

  val unmarked : ('v, 'e) t -> bool
  (** [unmarked vertex] is [true] iff [vertex] is unmarked *)

  val mark : ('v, 'e) t -> unit
  (** [mark vertex] marks [vertex] *)

  val has_unmarked_pred : ('v, 'e) t -> bool
  (** [has_marked_pred vertex] is [true] iff [vertex] has an unmarked
      predecessor *)

  val add_edge : src:('v, 'e) t -> dst:('v, 'e) t -> weight:'e -> unit
  (** [add_edge ~src ~dst ~weight] adds an edge [(src, dst)] with weight
      [weight] *)

  val set_edge : src:('v, 'e) t -> dst:('v, 'e) t -> weight:'e -> unit
  (** [set_edge ~src ~dst ~weight] removes any edge [(src, dst)] and
      adds a fresh one with weight [weight ]*)

  val remove_edge : src:('v, 'e) t -> dst:('v, 'e) t -> unit
  (** [remove_edge ~src ~dst] removes all edges [(src, dst)] *)

  val exists_incoming : ('v, 'e) t -> f:(('v, 'e) edge -> bool) -> bool
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

val graphviz :
  string_of_vertex:(('v, 'e) vertex -> string) ->
  string_of_weight:('e -> string) ->
  ('v, 'e) vertex list ->
  string
