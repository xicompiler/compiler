open Core

(** [Base] represents a base value in Xi *)
module Base : module type of struct
  include BaseValue
end

type base = Base.t
(** A [base] is a base primitive type in [Xi], either an int or a bool *)

type t =
  [ base
  | `Char of Uchar.t
  ]
(** A [t] represents a primitive int, bool, or char value in Xi *)

val cast : [< t ] -> base
(** [cast p] transforms [p] into a [base] primitive by transforming a
    [`Char] into an equivalent [`Int] *)

val typeof : [< t ] -> Type.Tau.primitive
(** [typeof p] is the primitive type of [p] *)

val sexp_of_t : [< t ] -> Sexp.t
(** [sexp_of_t prim] is the s-expression serialization of [prim] *)
