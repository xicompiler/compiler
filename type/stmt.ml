open! Core

type t =
  [ `Unit
  | `Void
  ]
[@@deriving sexp_of]

let to_string = function `Unit -> "Unit" | `Void -> "Void"
let lub t1 t2 = match (t1, t2) with `Void, `Void -> `Void | _ -> `Unit
