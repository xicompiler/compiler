open Core
module Base = BaseValue

type base = Base.t

type t =
  [ base
  | `Char of Uchar.t
  ]

let cast = function
  | `Char u -> `Int (u |> Uchar.to_scalar |> Int64.of_int)
  | #base as b -> b

let typeof = function `Int _ | `Char _ -> `Int | `Bool _ -> `Bool

(** [sexp_of_char c] is the s-expression serialization of character [c],
    surrounded by single quotes. *)
let sexp_of_char u =
  Sexp.Atom (u |> Unicode.to_string |> Printf.sprintf "'%s'")

let sexp_of_t = function
  | #base as b -> Base.sexp_of_t b
  | `Char u -> sexp_of_char u
