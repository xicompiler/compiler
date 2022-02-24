(** [S] is the signature of a Node *)
module type S = sig
  type 'a t
  (** ['a t] is the type of a node in the AST, parameterized by node
      type ['a] *)

  val get : 'a t -> 'a
  (** [get node] is the AST node wrapped in [Node] *)
end

module Ident : S with type 'a t = 'a
(** [Ident] is the type of an identity node *)
