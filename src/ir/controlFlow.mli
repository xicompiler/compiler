open! Core

module CFG : Graph.Directed.S with type Key.t = int
(** [CFG] is the type of a CFG in IR *)

val create_cfg : Lir.stmt list -> (Lir.stmt, unit) CFG.vertex list
(** [create_cfg stmts] is the control flow graph corresponding to the IR
    program [stmts] *)
