open Core

module type S = sig
  type 'a t

  val get : 'a t -> 'a
end

module Position = struct
  type 'a t = {
    value : 'a;
    position : Position.t;
  }

  let get { value } = value
  let position { position } = position
  let make ~pos value = { value; position = pos }
end