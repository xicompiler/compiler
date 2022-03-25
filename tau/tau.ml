open Core

type primitive = Primitive.t

module Primitive = Primitive

(** [poly_string] is the string representation of [`Poly] *)
let poly_string = "<poly>"

type t =
  [ primitive
  | `Array of t
  ]

let rec to_string = function
  | #primitive as p -> Primitive.to_string p
  | `Array t -> to_string t ^ "[]"

let rec sexp_of_t = function
  | `Array t -> Sexp.List [ Sexp.Atom "[]"; sexp_of_t t ]
  | #primitive as p -> Primitive.sexp_of_t p

let rec equal t1 t2 =
  match (t1, t2) with
  | `Poly, _ | _, `Poly | `Int, `Int | `Bool, `Bool -> true
  | `Array t1', `Array t2' -> equal t1' t2'
  | _ -> false
