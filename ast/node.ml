open Core

module type S = sig
  type 'a t

  val get : 'a t -> 'a
end

module Ident = struct
  type 'a t = 'a

  let get = Fn.id
end

module Pos = struct
  type 'a t = 'a * Position.t

  let get = fst
  let get_pos = snd
end
