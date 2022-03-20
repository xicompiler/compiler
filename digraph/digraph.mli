type ('v, 'e) vertex
(** A [('v, 'e) vertex] is a vertex carrying a value of type ['v] and
    adjacent to edges carrying type ['e] *)

type ('v, 'e) edge
(** A [('v, 'e) edge] is an edge carrying a value of type ['e] and
    incident to vertexs carrying type ['v] *)

(** [vertex] represents a vertex in a directed graph*)
module Vertex : sig
  type ('v, 'e) t = ('v, 'e) vertex
  (** A [('v, 'e) t] is a vertex carrying a value of type ['v] and
      adjacent to edges carrying type ['e] *)

  val incoming : ('v, 'e) t -> ('v, 'e) edge list
  (** [incoming vertex] is the list of edges entering [vertex] *)

  val outgoing : ('v, 'e) t -> ('v, 'e) edge list
  (** [outgoing vertex] is the list of exiting [vertex] *)

  val value : ('v, 'e) t -> 'v
  (** [value vertex] is the value stored by [vertex] *)
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
