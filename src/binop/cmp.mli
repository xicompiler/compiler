open Core

type t =
  [ Ord.t
  | Eq.t
  ]
[@@deriving hash, compare, sexp]
(** [t] represents a comparison operation on ints *)

val eval : [< t ] -> int64 -> int64 -> bool
(** [eval op i1 i2] is the result of the binary comparison operation
    [i1 op i2] *)
