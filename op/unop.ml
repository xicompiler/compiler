open Core
open Int64

type t =
  [ `IntNeg
  | `LogNeg
  ]

let to_string = function `IntNeg -> "-" | `LogNeg -> "!"

let eval uop x =
  match (uop, Primitive.cast x) with
  | `IntNeg, `Int i -> Some (`Int ~-i)
  | `LogNeg, `Bool b -> Some (`Bool (not b))
  | `IntNeg, `Bool _ | `LogNeg, `Int _ -> None
