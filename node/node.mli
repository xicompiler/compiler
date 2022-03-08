(** [S] is the signature of a Node *)
module type S = sig
  type 'a t
  (** ['a t] is the type of a node in the AST, parameterized by node
      type ['a] *)

  val get : 'a t -> 'a
  (** [get node] is the AST node wrapped in [Node] *)
end

(** [Position] is the type of a node with position information *)
module Position : sig
  include S

  val position : 'a t -> Position.t
  (** [position node] is the position wrapped by node [node] *)

  val make : pos:Position.t -> 'a -> 'a t
  (** [make ~pos value] is a [Position] carrying value [value] and
      position [position] *)

  val error : cause:'err -> 'a t -> 'err Position.error
  (** [error ~cause pos] is a position with cause [cause] occurring at
      position [pos] *)
end
