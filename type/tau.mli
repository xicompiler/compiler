open Core

type nonrec primitive =
  [ `Int
  | `Bool
  ]
(** A [primitive] is the type of a primitive value in Xi: either an
    integer or a boolean *)

type t =
  [ primitive
  | `Poly
  | `Array of t
  ]
(** A type in Xi is either a primitive type or an array of a type, where
    an Array is represented by a pair (contents, length) *)

val is_array : t -> bool
(** [is_array t] is [true] iff [t] is an array type. *)

val equal : t -> t -> bool
(** [equal t1 t2] is [true] iff [t1] and [t2] represent equivalent types *)
