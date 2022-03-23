open Subtype

type expr =
  [ expr Subtype.expr
  | expr Subtype.call
  | `ESeq of stmt * expr
  ]
(** An [expr] is a mid-level intermediate representation expression *)

and stmt =
  [ expr cjump2
  | `Seq of stmt list
  ]
(** A [stmt] is a mid-level intermediate representation statement *)

type toplevel =
  [ `Func of label * stmt list
  | `Data of label * Int64.t
  ]

val translate : Ast.Decorated.Toplevel.source -> toplevel list
(** [translate src] is decorated ast source [src] translated to
    mid-level IR *)
