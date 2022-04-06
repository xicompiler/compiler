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
  | `Data of label * int64 list
  ]

val mangle : Context.Error.id -> ctx:Context.t -> label
(** [mangle id ~ctx] is the mangled function name of [id] in context
    [ctx] *)

val commute : expr -> expr -> bool
(** [commute e1 e2] is [true] iff expressions [e1] and [e2] commute *)

val translate :
  gensym:IrGensym.t -> Ast.Decorated.source -> toplevel list
(** [translate ~gensym src] is decorated ast source [src] translated to
    mid-level IR, generating fresh symbols using [gensym] *)
