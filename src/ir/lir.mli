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

module CFG : Graph.Directed.S with type Key.t = int
(** [CFG] is the type of a CFG in IR *)

val create_cfg : stmt list -> (stmt, unit) CFG.vertex list
(** [create_cfg stmts] is the control flow graph corresponding to the IR
    program [stmts] *)

val live_out : stmt list -> int -> Temp.Virtual.Set.t
(** [live_out stmts i] is the set of variables that are live out of the
    [i]th statement of [stmts] *)

val dce : stmt list -> stmt list
(** [dce stmts] is [stmts] with dead definitions removed *)
