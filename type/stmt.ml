open! Core

type t =
  [ `Unit
  | `Void
  ]
[@@deriving sexp_of]

let lub t1 t2 = match (t1, t2) with `Void, `Void -> `Void | _ -> `Unit
