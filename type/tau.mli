open Core

type nonrec primitive =
  [ `Int
  | `Bool
  ]
[@@deriving sexp_of]
(** A [primitive] is the type of a primitive value in Xi: either an
    integer or a boolean *)

type t =
  [ primitive
  | `Poly
  | `Array of t
  ]
[@@deriving sexp_of]
(** A type in Xi is either a primitive type or an array of a type, where
    an Array is represented by a pair (contents, length) *)

val to_string : t -> string
(** [to_string t] is the string representing [t] *)

val int_array : t
(** [int_array] is [`Array `Int]*)

val is_array : t -> bool
(** [is_array t] is [true] iff [t] is an array type. *)

val equal : t -> t -> bool
(** [equal t1 t2] is [true] iff [t1] and [t2] represent equivalent types *)
