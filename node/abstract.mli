(** [Base] is the basic type of a node *)
module type Base = sig
  type 'a t
  (** ['a t] is the type of a node in the AST, parameterized by node
      type ['a] *)

  val get : 'a t -> 'a
  (** [get node] is the AST node wrapped in [Node] *)

  val set : value:'b -> 'a t -> 'b t
  (** [set ~value node] is [node'] such that [get node'] is [value] *)
end

(** [S] is the signature of a Node *)
module type S = sig
  include Base

  val compose : f:('a -> 'b) -> 'a t -> 'b
  (** [compose ~f node] is [f (get node)] *)

  val map : f:('a -> 'b) -> 'a t -> 'b t
  (** [map ~f node] is [node] with the value of [node] mapped by [f] *)
end
