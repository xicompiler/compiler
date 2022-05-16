open Core.Int64

type t =
  [ `And
  | `Or
  ]
[@@deriving hash, compare, sexp]

let eval = function `And -> ( && ) | `Or -> ( || )
let eval_bits = function `And -> ( land ) | `Or -> ( lor )
