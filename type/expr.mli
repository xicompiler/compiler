open Core

type t =
  [ Tau.t
  | `Tuple of Tau.t list
  ]
[@@deriving sexp_of]
(** [t] is the type of an expression node in Xi. *)

val equal : [< t ] -> [< t ] -> bool
(** [equal t1 t2] is [true] iff [t1] and [t2] represent the same
    expression type. *)
