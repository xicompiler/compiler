open Core

type nonrec primitive =
  [ `Int
  | `Bool
  ]
(** A [primitive] is the type of a primitive value in Xi: either an
    integer or a boolean *)

type t =
  [ primitive
  | `Array of t
  ]
(** A type in Xi is either a primitive type or an array of a type, where
    an Array is represented by a pair (contents, length) *)

val array : t -> t
(** [array t] is [`Array t] *)
