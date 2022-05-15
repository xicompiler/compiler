open Core
open Option.Let_syntax

type primitive = Primitive.t

module Primitive = Primitive

(** [poly_string] is the string representation of [`Bot] *)
let poly_string = "<poly>"

type t =
  [ primitive
  | `Array of t
  | `Record of string
  ]

let rec to_string = function
  | #primitive as p -> Primitive.to_string p
  | `Array t -> to_string t ^ "[]"
  | `Record s -> s

let rec sexp_of_t = function
  | `Array t -> Sexp.List [ Sexp.Atom "[]"; sexp_of_t t ]
  | `Record s -> Sexp.Atom s
  | #primitive as p -> Primitive.sexp_of_t p

let rec join t1 t2 =
  match (t1, t2) with
  | t, `Bot | `Bot, t -> Some t
  | `Record r1, `Record r2 ->
      if String.equal r1 r2 then Some (`Record r1) else None
  | `Null, `Record r1 | `Record r1, `Null -> Some (`Record r1)
  | `Record _, _ | _, `Record _ -> None
  | `Null, `Array t1 | `Array t1, `Null -> Some (`Array t1)
  | `Null, `Null -> Some `Null
  | `Null, #Primitive.base | #Primitive.base, `Null -> None
  | `Array _, #Primitive.base | #Primitive.base, `Array _ -> None
  | (#Primitive.base as t1'), (#Primitive.base as t2') ->
      if Primitive.equal t1' t2' then Some t1 else None
  | `Array t1, `Array t2 ->
      let%map t = join t1 t2 in
      `Array t

let equal t1 t2 = Option.is_some (join t1 t2)
