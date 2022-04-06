open Core
open Option.Let_syntax

type primitive = Primitive.t

module Primitive = Primitive

(** [poly_string] is the string representation of [`Bot] *)
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

let rec join t1 t2 =
  match (t1, t2) with
  | t, `Bot | `Bot, t -> Some t
  | `Array _, #Primitive.base | #Primitive.base, `Array _ -> None
  | (#Primitive.base as t1'), (#Primitive.base as t2') ->
      if Primitive.equal t1' t2' then Some t1 else None
  | `Array t1, `Array t2 ->
      let%map t = join t1 t2 in
      `Array t

let equal t1 t2 = Option.is_some (join t1 t2)
