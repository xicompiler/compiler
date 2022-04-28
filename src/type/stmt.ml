open Core

type t =
  [ `Unit
  | `Void
  ]
[@@deriving sexp_of, variants]

let to_string = Variants.to_name
let equal = Poly.equal
let lub t1 t2 = match (t1, t2) with `Void, `Void -> `Void | _ -> `Unit
