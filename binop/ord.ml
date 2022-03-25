open Core
open Int64

type t =
  [ `Lt
  | `Leq
  | `Geq
  | `Gt
  ]

let eval = function
  | `Lt -> ( < )
  | `Leq -> ( <= )
  | `Geq -> ( >= )
  | `Gt -> ( > )
