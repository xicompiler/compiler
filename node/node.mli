(** [S] is the signature of a Node *)
module type S = sig
  type 'a t
  (** ['a t] is the type of a node in the AST, parameterized by node
      type ['a] *)

  val get : 'a t -> 'a
  (** [get node] is the AST node wrapped in [Node] *)
end

(** [Context] represents a node containing a static context *)
module type Context = sig
  include S

  type typ
  (** [typ] is the type of a value wrapped in a node *)

  type context
  (** [context] is the type of the context wrapped in a node *)

  type nonrec 'a result = ('a, Type.error Position.error) result
  (** An ['a result] is either [Ok 'a] or [Error err], where [err]
      describes the cause and type of a semantic error. *)

  val context : 'a t -> context
  (** [context node] is the context of node *)

  val typ : 'a t -> typ
  (** [typ v] is the type of the value wrapped in [v] *)

  val make : 'a -> ctx:context -> typ:typ -> 'a t
  (** [make v ~ctx ~typ] is a node wrapping value [v] with context [ctx]
      and type [typ] *)
end

(** [Position] is the type of a node with position information *)
module Position : sig
  include S

  val position : 'a t -> Position.t
  (** [position node] is the position wrapped by node [node] *)

  val make : position:Position.t -> 'a -> 'a t
  (** [make ~position value] is a [Position] carrying value [value] and
      position [position] *)
end
