open Core

type t =
  [ `Eq
  | `Neq
  ]
[@@deriving hash, compare, sexp]

let eval ~equal op i1 i2 =
  match op with `Eq -> equal i1 i2 | `Neq -> not (equal i1 i2)
