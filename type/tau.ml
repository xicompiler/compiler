open Core

type nonrec primitive =
  [ `Int
  | `Bool
  ]

type t =
  [ primitive
  | `Poly
  | `Array of t
  ]

let rec equal t1 t2 =
  match (t1, t2) with
  | `Poly, _
  | _, `Poly
  | `Int, `Int
  | `Bool, `Bool ->
      true
  | `Array t1', `Array t2' -> equal t1' t2'
  | _ -> false

let is_array = function
  | `Array _ -> true
  | `Poly
  | `Int
  | `Bool ->
      false
