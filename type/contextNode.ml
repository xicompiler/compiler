module type S = sig
  include Node.S

  val context : 'a t -> Context.t
  (** [context node] is the context of node *)

  val position : 'a t -> Position.t
  (** [position node] is the position corresponding to [node] *)
end