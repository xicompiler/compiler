open! Core

type t =
  [ Expr.t
  | `Unit
  ]
[@@deriving sexp_of]
(** [t] is a type in procedures, functions, and multiple assignments. *)

val equal : [< t ] -> [< t ] -> bool
(** [equal t1 t2] is [true] iff [t1] and [t2] represent equivalent term
    types. *)

val or_unit : [< t ] option -> t
(** [or_unit opt] is [t :> Term.t] if [opt] is [Some t] and [`Unit]
    otherwise *)
