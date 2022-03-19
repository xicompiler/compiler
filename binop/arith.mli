open Core

type base =
  [ `Mult
  | `Plus
  | `Minus
  | `Div
  | `Mod
  ]
(** [base] represents all operations executable on a processor *)

type t =
  [ base
  | `HighMult
  ]
(** A [t] is the type of an arithemtic binop *)

val high_mult : int64 -> int64 -> int64
(** [high_mult i1 i2] is the higher 64 bits of the 128 bit product
    [i1 * i2] *)

val eval_exn : [< t ] -> int64 -> int64 -> int64
(** [eval_exn op i1 i2] is the binary operation [i1 op i2]. Raises an
    exception on failure. *)

val eval : [< t ] -> int64 -> int64 -> int64 option
(** [eval_total op i1 i2] is the result of the binary operation
    [i1 op i2] if successful, and [None] on failure *)
