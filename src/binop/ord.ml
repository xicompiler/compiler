open Core
open Int64

type t =
  [ `Lt
  | `Leq
  | `Geq
  | `Gt
  ]
[@@deriving hash, compare, sexp]

let eval = function
  | `Lt -> ( < )
  | `Leq -> ( <= )
  | `Geq -> ( >= )
  | `Gt -> ( > )
