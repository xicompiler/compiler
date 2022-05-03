open Core

module type Params2 = sig
  module Key : sig
    type t [@@deriving compare]
  end

  type ('a, 'b) t

  val create : key:Key.t -> value:'a -> ('a, 'b) t
  val key : ('a, 'b) t -> Key.t
  val set : ('v, 'e) t -> value:'v -> unit
  val value : ('a, 'b) t -> 'a
  val marked : ('v, 'e) t -> bool
  val mark : ('v, 'e) t -> unit
end

module type S2 = sig
  include Params2

  val fold : ('v, 'e) t -> f:('v -> 'a) -> 'a
  val update : ('v, 'e) t -> f:('v -> 'v) -> unit
  val unmarked : ('v, 'e) t -> bool
  val equal : ('v, 'e) t -> ('v, 'e) t -> bool
end

(** A helper functor that produces all of the auxilliary functions
    applicable to vertices *)
module Make2Aux (Args : Params2) = struct
  open Args

  let fold v ~f = f (value v)
  let update v ~f = set v ~value:(fold v ~f)
  let unmarked v = not (marked v)
  let equal u v = Key.compare (key u) (key v) = 0
end

module Make2 (Args : Params2) = struct
  include Args
  include Make2Aux (Args)
end

module type S = sig
  type 'a t

  include S2 with type ('a, 'b) t := 'a t
end

module type Params = sig
  type 'a t

  include Params2 with type ('a, 'b) t := 'a t
end

module Make (Args : Params) = struct
  include Make2Aux (struct
    include Args

    type ('a, 'b) t = 'a Args.t
  end)

  include Args
end
