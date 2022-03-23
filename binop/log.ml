open Core.Int64

type t =
  [ `And
  | `Or
  ]

let eval = function `And -> ( && ) | `Or -> ( || )
let eval_bits = function `And -> ( land ) | `Or -> ( lor )
