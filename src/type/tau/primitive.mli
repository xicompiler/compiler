type base =
  [ `Int
  | `Bool
  ]
(** [base] is a primitive type expressible in Xi *)

type t =
  [ base
  | `Null
  | `Bot
  ]
[@@deriving sexp_of]
(** A [t] is the type of a primitive value in Xi: either an integer or a
    boolean, or a polymorphic type *)

val equal : [< t ] -> [< t ] -> bool
(** [equal t1 t2] is [true] iff [t1] and [t2] represent equivalent types *)

include Util.Stringable.S with type t := t
