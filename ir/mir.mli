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

val translate : Ast.Decorated.t -> stmt
(** [translate ast] is decorated ast [ast] translated to lowered
    (canonical) IR *)
