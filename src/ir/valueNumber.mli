open! Core

val number : Lir.t -> Lir.t
(** [number stmts] is [stmts] transformed by local value numbering *)
