open Subtype

type expr = expr Subtype.expr
(** An [expr] is a low-level intermediate representation expression *)

type stmt =
  [ expr Subtype.stmt
  | `Call of expr * expr list
  | `CJump of expr * label
  ]
(** A [stmt] is a low-level intermediate representation statement *)
