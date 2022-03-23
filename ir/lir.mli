open Subtype

type expr = expr Subtype.expr
(** An [expr] is a low-level intermediate representation expression *)

type stmt = expr Subtype.cjump2
(** A [stmt] is a low-level intermediate representation statement *)

val log_neg : expr -> expr
(** [log_neg expr] is the logical negation of [expr] *)

type t = stmt list
(** [t] is the representation of a program in lowered IR, a list of
    statement s*)
