open Core

module type Key = Hashtbl.Key
(** [Key] is the type of a module wrapping the index of a ndoe *)

(** [S] is the signature of a mutable, directed graph *)
module type S = sig
  module Key : Key
  (** [Key] is the type of keys indexing nodes *)

  type key = Key.t
  (** [key] is the type of a key indexing nodes *)

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

    val incoming : ('v, 'e) t -> ('v, 'e) edge list
    (** [incoming vertex] is the list of edges entering [vertex] *)

    val pred : ('v, 'e) t -> ('v, 'e) t list
    (** [pred vertex] is the list of predecessor nodes of [vertex] *)

    val outgoing : ('v, 'e) t -> ('v, 'e) edge list
    (** [outgoing vertex] is the list of exiting [vertex] *)

    val succ : ('v, 'e) t -> ('v, 'e) t list
    (** [succ vertex] is the list of successor nodes of [vertex] *)

    val key : ('v, 'e) t -> key
    (** [key vertex] is the index of [vertex] *)

    val value : ('v, 'e) t -> 'v
    (** [value vertex] is the value stored by [vertex] *)

    val marked : ('v, 'e) t -> bool
    (** [marked vertex] is [true] iff [vertex] has been marked *)

    val mark : ('v, 'e) t -> unit
    (** [mark vertex] marks [vertex] *)

    val marked_pred : ('v, 'e) t -> bool
    (** [marked_pred vertex] is [true] iff each of the predecessor
        vertices of [vertex] are marked *)

    val add_edge : src:('v, 'e) t -> dst:('v, 'e) t -> weight:'e -> unit
    (** [add_edge ~src ~dst ~weight] adds an edge [(src, dst)] with
        weight [weight] *)
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
  (** A [('v, 'e) t] represents a mutable, directed graph whose nodes
      are indexed by type [key] and store type ['v], and carry edges
      with weight of type ['e] *)

  val create : ?size:int -> unit -> ('v, 'e) t
  (** [create ~size ()] is a fresh [t] with capacity [size] *)

  val set_vertex : ('v, 'e) t -> key:key -> value:'v -> unit
  (** [add_vertex g ~key ~value] sets the vertex with index [key] to a
      fresh unmarked vertex with value [value] in [g] with no incident
      edges *)

  val find_vertex : ('v, 'e) t -> key:key -> ('v, 'e) vertex option
  (** [find_vertex ~key g] is [Some v] where [v] is the vertex bound to
      [key] in [g] if present, or [None] otherwise *)

  val find_vertex_exn : ('v, 'e) t -> key:key -> ('v, 'e) vertex
  (** Same as [find_vertex] but raises on [None] *)
end
