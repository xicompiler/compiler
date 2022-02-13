(** A [primitive] is the type of a primitive value in Xi: either an
    integer or a boolean *)
type nonrec primitive =
  | Int
  | Bool

(** A type in Xi is either a primitive type or an array of a type *)
type t =
  | Primitive of primitive
  | Array of t

val to_string : t -> string(** [to_string t] is [t] formatted as a string *)
