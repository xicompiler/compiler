(** [Make (Base)] is a [Node] wrapping the same type as [Base] *)
module Make (B : Abstract.Base) : Abstract.S with type 'a t = 'a B.t
