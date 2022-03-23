type expr =
  [ expr Subtype.expr
  | expr Subtype.call
  | `ESeq of stmt * expr
  ]
(** An [expr] is a mid-level intermediate representation expression *)

and stmt =
  [ expr Subtype.cjump2
  | `Seq of stmt list
  ]
(** A [stmt] is a mid-level intermediate representation statement *)
