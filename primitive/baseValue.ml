open Core
open Int64

type t =
  [ `Int of int64
  | `Bool of bool
  ]
[@@deriving variants]

let sexp_of_t = function
  | `Bool b -> Bool.sexp_of_t b
  | `Int i ->
      if is_negative i then
        let i = if i = min_value then max_value else ~-i in
        Sexp.List [ Sexp.Atom "-"; Int64.sexp_of_t i ]
      else Int64.sexp_of_t i
