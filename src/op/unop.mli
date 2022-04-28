open Core

type t =
  [ `IntNeg
  | `LogNeg
  ]
(** [t] is the type of a unary operator *)

val to_string : [< t ] -> string
(** [to_string unop] is the string representation of [unop]. *)

val eval : [< t ] -> [< Primitive.t ] -> Primitive.base option
(** [eval uop x] if [Some (uop x)], where [uop x] is the unary operator
    [uop] applied to [x], if the type of [uop] is applicable to [x] and
    [None] otherwise. *)
