open Core
open Int64
open Big_int

type t =
  [ `Int of int64
  | `Bool of bool
  ]
[@@deriving variants]

let sexp_of_t = function
  | `Bool b -> Bool.sexp_of_t b
  | `Int i ->
      if is_negative i then
        let i = i |> big_int_of_int64 |> minus_big_int in
        Sexp.List [ Sexp.Atom "-"; Sexp.Atom (string_of_big_int i) ]
      else Int64.sexp_of_t i
