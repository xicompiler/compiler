open Core

type t =
  [ `Int
  | `Bool
  | `Poly
  ]
(** A [t] is the type of a primitive value in Xi: either an integer or a
    boolean, or a polymorphic type *)

val to_string : t -> string
(** [to_string typ] is the string representation of [typ] *)

val sexp_of_t : t -> Sexp.t
(** [sexp_of_t p] is the s-expression serialization of primitive type
    [p] *)
