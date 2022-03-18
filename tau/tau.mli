open Core

type primitive = Primitive.t
(** A [primitive] is the type of a primitive value in Xi: either an
    integer or a boolean *)

(** [Primitive] represents a primitive type in Xi *)
module Primitive : module type of struct
  include Primitive
end

type t =
  [ primitive
  | `Array of t
  ]
(** A type in Xi is either a primitive type or an array of a type, where
    an Array is represented by a pair (contents, length) *)

val to_string : t -> string
(** [to_string t] is the string representing [t] *)

val sexp_of_t : t -> Sexp.t
(** [sexp_of_t typ] is the s-expression serialization of [typ] *)

val is_array : t -> bool
(** [is_array t] is [true] iff [t] is an array type. *)

val equal : t -> t -> bool
(** [equal t1 t2] is [true] iff [t1] and [t2] represent equivalent types *)