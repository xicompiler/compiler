(** A [primitive] is the type of a primitive value in Xi: either an
    integer or a boolean *)
type nonrec primitive =
  | Int
  | Bool

(** A type in Xi is either a primitive type or an array of a type *)
type 'a t =
  | Primitive of primitive
  | Array of 'a array

and 'a array = {
  contents : 'a t;
  length : 'a option;
}
(** An ['a array] is the type of a Xi array whose contentes have type
    ['a t] and that can optionally be initialized by sizes of type ['a] *)

val to_string : 'a t -> string
(** [to_string t] is [t] formatted as a string *)

val equal : 'a t -> 'a t -> bool
(** [equal t1 t2] is [true] iff [t1] and [t2] represent equal Xi types *)
