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

val join : t -> t -> t option
(** [join t1 t2] is [Some t] where [t] is the least upper bound of [t1]
    and [t2], assuming [`Bot] is a subtype of every other type, or
    [None] if the join of [t1] and [t2] is top *)

include Util.Stringable.S with type t := t
include Equal.S with type t := t

val sexp_of_t : t -> Sexp.t
(** [sexp_of_t typ] is the s-expression serialization of [typ] *)
