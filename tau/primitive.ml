open Core

type t =
  [ `Int
  | `Bool
  | `Poly
  ]

let to_string = function
  | `Int -> "int"
  | `Bool -> "bool"
  | `Poly -> "<poly>"

let sexp_of_t typ = Sexp.Atom (to_string typ)
