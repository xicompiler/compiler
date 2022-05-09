open Core

(** [S2] is a graph with edge and vertex labels *)
module type S2 = sig
  type ('a, 'b) vertex
  (** An [('a, 'b) vertex] is a vertex with value of type ['a], adjacent
      to edges with weight ['b] *)

  type ('a, 'b) t
  (** An [('a, 'b) t] is the type of a graph whose vertices have
      vertices with value ['a] and edges with weights of type ['b] *)

  val add_vertex : ('a, 'b) t -> ('a, 'b) vertex -> unit
  (** [add_vertex g v] adds vertex [v] to graph [g]. Requires: there
      exists no vertex with the same key as [v] bound in [g] *)

  val create : ?size:int -> unit -> ('a, 'b) t
  (** [create ?size ()] is a fresh graph with initial capacity for
      [size] vertices *)

  val iter_vertices : ('v, 'e) t -> f:(('v, 'e) vertex -> unit) -> unit
  (** [iter_vertices g ~f] applies [f] to each of the vertices of [g] *)

  val of_vertices : ('v, 'e) vertex list -> ('v, 'e) t
  (* [of_vertices vs] is a graph containing each of the vertices in
     [vs]. Requires: each of the vertices in [vs] have distinct keys. *)
end

(** [Params2] contains the values needed to construct a graph with
    vertex values and edge weights *)
module type Params2 = sig
  type ('a, 'b) vertex
  (** An [('a, 'b) vertex] is a vertex with value of type ['a], adjacent
      to edges with weight ['b] *)

  module Table : Hashtbl.S
  (** A [Table] is used by the graph to store vertices *)

  val key : ('a, 'b) vertex -> Table.key
  (** [key v] is the unique key of vertex [v] *)
end

(** [Make2 (Args)] is an [S2] with the same vertices as [Args] *)
module Make2 (Args : Params2) :
  S2
    with type ('a, 'b) vertex := ('a, 'b) Args.vertex
    with type ('a, 'b) t = ('a, 'b) Args.vertex Args.Table.t

(** [S] represented an unweighted graph with vertex labels *)
module type S = sig
  type 'a vertex
  (** An ['a vertex] is a vertex with value of type ['a] *)

  type 'a t
  (** An ['a t] is the type of a graph whose vertices have vertices with
      value ['a] *)

  include
    S2
      with type ('a, 'b) vertex := 'a vertex
      with type ('a, 'b) t := 'a t
end

(** [Params] contains the values needed to construct an unweighted graph *)
module type Params = sig
  type 'a vertex
  (** An ['a vertex] is a vertex with value of type ['a] *)

  include Params2 with type ('a, 'b) vertex := 'a vertex
end

(** [Make (Args)] is an [S] with the same vertices as [Args] *)
module Make (Args : Params) :
  S
    with type 'a vertex := 'a Args.vertex
    with type 'a t = 'a Args.vertex Args.Table.t
