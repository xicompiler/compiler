open Subtype

type expr =
  [ expr Subtype.expr
  | `Call of expr * expr list
  | `ESeq of stmt * expr
  ]
(** An [expr] is a mid-level intermediate representation expression *)

and stmt =
  [ expr Subtype.stmt
  | `CallStmt of expr * expr list
  ]
(** A [stmt] is a mid-level intermediate representation statement *)
