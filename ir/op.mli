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

type negatable =
  [ `Lt
  | `Leq
  | `Geq
  | `Gt
  | `Eq
  | `Neq
  ]
(** [negatable] is a negatable operator in IR *)

val coerce : [< t ] -> t
(** [coerce bop] coerces [bop] to a [t] *)

val log_neg : negatable -> negatable
(** [log_neg op] is the negated operator of [op] *)

val eval : t -> int64 -> int64 -> int64 option

include Util.Stringable.S with type t := t
