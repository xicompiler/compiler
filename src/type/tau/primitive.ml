open Core

type base =
  [ `Int
  | `Bool
  ]

type t =
  [ base
  | `Bot
  ]

let to_string = function
  | `Int -> "int"
  | `Bool -> "bool"
  | `Bot -> "<poly>"

let equal t1 t2 =
  match (t1, t2) with
  | _, `Bot | `Bot, _ | `Int, `Int | `Bool, `Bool -> true
  | `Int, `Bool | `Bool, `Int -> false

let sexp_of_t typ = Sexp.Atom (to_string typ)
