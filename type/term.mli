open! Core

type t =
  [ Expr.t
  | `Unit
  ]
[@@deriving sexp_of]
(** [t] is a type in procedures, functions, and multiple assignments. *)

val of_tau_list : Tau.t list -> t
(** [of_tau_list e] is [e] converted from a [tau list] to a [term] *)

val to_tau_list : [< t ] -> Tau.t list
(** [to_tau_list e] is [e] converted from a [term] to a [tau list] *)

val to_expr : [< t ] -> Expr.t
(** [to_expr e] is [e] converted from a [term] to an [expr] type *)

val equal : [< t ] -> [< t ] -> bool
(** [equal t1 t2] is [true] iff [t1] and [t2] represent equivalent term
    types. *)

val or_unit : [< t ] option -> t
(** [or_unit opt] is [t :> Term.t] if [opt] is [Some t] and [`Unit]
    otherwise *)
