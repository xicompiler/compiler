open Core
open Int64

type total =
  [ `Mult
  | `HighMult
  | `Plus
  | `Minus
  ]

type partial =
  [ `Div
  | `Mod
  ]

type t =
  [ total
  | partial
  ]

let high_mult i1 i2 =
  let i1 = Big_int.big_int_of_int64 i1 in
  let i2 = Big_int.big_int_of_int64 i2 in
  let prod = Big_int.mult_big_int i1 i2 in
  let high = Big_int.shift_right_big_int prod 64 in
  Big_int.int64_of_big_int high

let eval_exn = function
  | `Mult -> ( * )
  | `HighMult -> high_mult
  | `Plus -> ( + )
  | `Minus -> ( - )
  | `Div -> ( / )
  | `Mod -> ( % )

let eval_total (op : total) = eval_exn op
let eval op i1 i2 = Option.try_with (fun () -> eval_exn op i1 i2)
