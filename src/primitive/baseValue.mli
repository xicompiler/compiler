open Core

type t =
  [ `Int of int64
  | `Bool of bool
  ]
[@@deriving variants]
(** A [t] is a base primitive type in [Xi], either an int or a bool *)

val sexp_of_t : t -> Sexp.t
(** [sexp_of_t base] is the s-expression serialization of [base] *)
