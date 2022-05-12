open Core

module type Key = Map.Key
(** [Key] is the unique key of a vertex in a graph *)

(** [S] is the type of an unwweighted, undirected graph *)
module type S = sig
  module Key : Key
  (** [Key] represents the unique key of a vertex *)

  type t
  (** [t] is the type of an undirected graph with unweighted edges *)

  val empty : t
  (** [empty] is a graph with no vertices *)

  val add_vertex : t -> Key.t -> t
  (** [add_vertex g k] is [g] with an additional vertex with key [k]
      with no incident edges, or just [g] if a vertex with key [k]
      already exists in [g] *)

  val add_edge : t -> Key.t -> Key.t -> t
  (** [add_edge g u v] is [g] with the undirected edge [{u, v}]. Raises
      if either [u] or [v] are unbound in [g] *)

  val kempe :
    ?precolor:(Key.t -> int option) ->
    t ->
    max:int ->
    (Key.t -> int, Key.t list) result
  (** [kempe ?precolor g ~max] is a function [Ok f] such that [f k] is
      is the color assigned the vertex with unique key [k] given [max]
      total colors, or [Error spills] where each of the vertices in
      [spills] could not be colored. *)

  val of_edges : (Key.t * Key.t) list -> t
  (** [of_edges \[\[(u1, v1); ...; (un, vn)\]\]] is a graph with all of
      the edges [u1, v1), ..., (un, vn)]. Unbound vertices are added as
      needed. *)
end

(** [Make (Key)] is an [S], an undirected, unweighted graph, where
    vertices are indexed with [Key.t] *)
module Make (Key : Key) : S with module Key = Key
