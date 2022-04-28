open Subtype

type expr = expr Subtype.expr
(** An [expr] is a low-level intermediate representation expression *)

type dest = expr Subtype.dest
(** A [dest] is an expression that can be moved into *)

type stmt = expr Subtype.cjump2
(** A [stmt] is a low-level intermediate representation statement *)

type toplevel =
  [ `Func of label * stmt list * int * int
  | `Data of label * int64 list
  ]

val log_neg : expr -> expr
(** [log_neg expr] is the logical negation of [expr] *)

type t = toplevel list
(** [t] is the representation of a program in lowered IR, a list of
    statement s*)

val lower : gensym:IrGensym.t -> Mir.toplevel list -> t
(** [lower stmt] is the lowered form of mir statement [stmt] *)
