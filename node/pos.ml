open Core

module B = struct
  type 'a t = {
    value : 'a;
    position : Position.t;
  }

  let get { value } = value
  let set ~value node = { node with value }
  let foo = 5
end

include Factory.Make (B)
open B

let position { position } = position
let make ~pos value = { value; position = pos }
let error ~cause { position } = Position.Error.make ~pos:position cause
