open Core

type nonrec primitive =
  [ `Int
  | `Bool
  ]

type t =
  [ primitive
  | `Array of t
  ]

let array t = `Array t
