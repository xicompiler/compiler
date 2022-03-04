open Core

type nonrec primitive =
  [ `Int
  | `Bool
  ]

type t =
  [ primitive
  | `Array of t
  ]

let equal = Poly.equal

let is_array = function
  | `Array _ -> true
  | `Int
  | `Bool ->
      false
