open Core

module type Key = Hashtbl.Key
(** [Key] is the unique key of a vertex in a graph *)

(** [S] is the type of an unwweighted, undirected graph *)
module type S = sig
  module Key : Key
  (** [Key] represents the unique key of a vertex *)

  (** [Vertex] represents a vertex in an undirected graph *)
  module Vertex : sig
    include Vertex.S with module Key := Key

    val foldi_adjacent :
      'a t -> init:'acc -> f:(Key.t -> 'acc -> 'a t -> 'acc) -> 'acc
    (** Same as [fold_adjacent], but [f] takes in the unique key of each
        adjacent vertex *)

    val fold_adjacent :
      'a t -> init:'acc -> f:('acc -> 'a t -> 'acc) -> 'acc
    (** [fold_adjacent v ~init ~f] folds [f] over all the vertices
        adjacent to [v], starting with initial value [init] *)

    val iter_adjacent : 'a t -> f:('a t -> unit) -> unit
    (** [iter_adjacent v ~f] applies [f] to each of the vertices
        adjacent to [v] *)

    val add_edge : 'a t -> 'a t -> unit
    (** [add_edge u v] constructs an unweighted, undirected edge
        [(u, v)] *)
  end

  include Creators.S with type 'a vertex := 'a Vertex.t
end

(** [Make (Key)] is an [S], an undirected, unweighted graph, where
    vertices are indexed with [Key.t] *)
module Make (Key : Key) : S with module Key = Key
