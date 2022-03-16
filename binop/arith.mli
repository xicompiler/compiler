open Core

type total =
  [ `Mult
  | `HighMult
  | `Plus
  | `Minus
  ]
(** A [total] is a binary operation that is a total function from R^2 to
    R *)

type partial =
  [ `Div
  | `Mod
  ]
(** A [partial] is a binary operation that is a partial function from
    R^2 to R *)

type t =
  [ total
  | partial
  ]
(** A [t] is the type of an arithemtic binop *)

val high_mult : int64 -> int64 -> int64
(** [high_mult i1 i2] is the higher 64 bits of the 128 bit product
    [i1 * i2] *)

val eval_exn : [< t ] -> int64 -> int64 -> int64
(** [eval_exn op i1 i2] is the binary operation [i1 op i2]. Raises an
    exception on failure. *)

val eval_total : total -> int64 -> int64 -> int64
(** [eval_total op i1 i2] is the result of the binary operation
    [i1 op i2] *)

val eval : [< t ] -> int64 -> int64 -> int64 option
(** [eval_total op i1 i2] is the result of the binary operation
    [i1 op i2] if successful, and [None] on failure *)
