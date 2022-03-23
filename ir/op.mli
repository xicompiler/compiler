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
  | unsigned
  | Binop.t
  ]
(** [t] is the type of an operator in IR *)

val coerce : [< t ] -> t
(** [coerce bop] coerces [bop] to a [t] *)

val to_string : t -> string
(** [to_string op] converts [op] to a string *)

val eval : t -> int64 -> int64 -> int64 option
