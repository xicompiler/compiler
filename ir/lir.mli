open Subtype

type expr = expr Subtype.expr
(** An [expr] is a low-level intermediate representation expression *)

type stmt = expr Subtype.cjump2
(** A [stmt] is a low-level intermediate representation statement *)

type toplevel =
  [ `Func of label * stmt list
  | `Data of label * Int64.t
  ]

val log_neg : expr -> expr
(** [log_neg expr] is the logical negation of [expr] *)

type t = toplevel list
(** [t] is the representation of a program in lowered IR, a list of
    statement s*)

val lower : Mir.toplevel list -> t
(** [lower stmt] is the lowered form of mir statement [stmt] *)
