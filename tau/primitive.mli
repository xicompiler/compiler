open Core

type t =
  [ `Int
  | `Bool
  | `Poly
  ]
[@@deriving sexp_of]
(** A [t] is the type of a primitive value in Xi: either an integer or a
    boolean, or a polymorphic type *)

include Util.Stringable.S with type t := t
