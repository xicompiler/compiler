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

val map_stmt : f:(Temp.Virtual.t -> [< expr ]) -> stmt -> stmt
(** [map_temp ~f stmt] is [stmt] with [f] applied to all temps *)

val def : ?init:Temp.Virtual.Set.t -> stmt -> Temp.Virtual.Set.t
(** [def ?init stmt] is the set of defined variables in stmt *)

val use : ?init:Temp.Virtual.Set.t -> stmt -> Temp.Virtual.Set.t
(** [use ?init stmt] is the set of used variables in stmt *)

val lower : opt:Opt.t -> gensym:IrGensym.t -> Mir.toplevel list -> t
(** [lower ~opt stmt] is the lowered form of mir statement [stmt] *)

module CFG : Graph.Directed.S with type Key.t = int
(** [CFG] is the type of a CFG in IR *)

val create_cfg : stmt list -> (stmt, unit) CFG.vertex list
(** [create_cfg stmts] is the control flow graph corresponding to the IR
    program [stmts] *)

val to_string : stmt -> string
(** [to_string stmt] is the string representation of [stmt] *)
