type log =
  [ Binop.log
  | `Xor
  ]
(** [log] is the type of a logical operator in IR *)

type arith = Binop.Arith.base
(** [arith] is the type of an arithmetic operator in IR *)

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
  [ log
  | arith
  | unsigned
  | Binop.cmp
  ]
(** [t] is the type of an operator in IR *)
