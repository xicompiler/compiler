open Subtype

type expr =
  [ `Call of expr * expr list
  | `ESeq of stmt * expr
  | expr Subtype.expr
  ]
(** An [expr] is a mid-level intermediate representation expression *)

and stmt =
  [ expr Subtype.stmt
  | `Seq of stmt list
  | `CJump of expr * label * label
  ]
(** A [stmt] is a mid-level intermediate representation statement *)
