open Core

type t =
  [ `Lt
  | `Leq
  | `Geq
  | `Gt
  ]
(** A [t] is a binary operator representing a partial order on integers *)

val eval : t -> int64 -> int64 -> bool
(** [eval cmp i1 i2] is the result of the comparison [i1 cmp i2] *)
