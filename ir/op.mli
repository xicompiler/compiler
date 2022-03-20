type bitwise = [ `Xor ]
(** [bitwise] is the type of a bitwise operator in IR *)

type shift =
  [ `LShift
  | `RShift
  | `ARShift
  ]
(** [shift] is the type of a logical shift operator *)

type unsigned =
  [ `ULt
  | `ULeq
  | `UGt
  | `UGeq
  ]
(** [unsigned] is the type of an unsigned comparison operator *)

type t =
  [ bitwise
  | shift
  | unsigned
  | Binop.t
  ]
(** [t] is the type of an operator in IR *)

val coerce : [< t ] -> t
(** [coerce bop] coerces [bop] to a [t] *)
