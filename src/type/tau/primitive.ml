open Core

type base =
  [ `Int
  | `Bool
  ]

type t =
  [ base
  | `Null
  | `Bot
  ]

let to_string = function
  | `Int -> "int"
  | `Bool -> "bool"
  | `Null -> "null"
  | `Bot -> "<poly>"

let equal t1 t2 =
  match (t1, t2) with
  | _, `Bot | `Bot, _ | `Int, `Int | `Null, `Null | `Bool, `Bool -> true
  | `Int, `Bool | `Bool, `Int | `Null, `Bool | `Bool, `Null 
  | `Null, `Int | `Int, `Null -> false

let sexp_of_t typ = Sexp.Atom (to_string typ)
