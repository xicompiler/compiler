open Core

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

val sexp_of_t : 'a t -> Sexp.t
(** [sexp_of_t t] is the s-expression serialization of [t] *)
