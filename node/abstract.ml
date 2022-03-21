module type Base = sig
  type 'a t

  val get : 'a t -> 'a
  val set : value:'b -> 'a t -> 'b t
end

module type S = sig
  include Base

  val compose : f:('a -> 'b) -> 'a t -> 'b
  val map : f:('a -> 'b) -> 'a t -> 'b t
end
