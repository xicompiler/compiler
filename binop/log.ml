type t =
  [ `And
  | `Or
  ]

let eval = function `And -> ( && ) | `Or -> ( || )
