open Subtype
open Ast.Decorated

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

type dest = expr Subtype.dest
(** A [dest] is an expression that can be moved into *)

type toplevel =
  [ `Func of label * stmt list
  | `Data of label * int64
  ]

val commute : expr -> expr -> bool
(** [has_mem e] is [true] iff expression [e] contains a [`Mem] node in
    its expression tree *)

val translate : gensym:IrGensym.t -> Toplevel.source -> toplevel list
(** [translate ~gensym src] is decorated ast source [src] translated to
    mid-level IR, generating fresh symbols using [gensym] *)
