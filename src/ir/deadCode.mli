val live_out : Lir.stmt list -> int -> Temp.Virtual.Set.t
(** [live_out stmts i] is the set of variables that are live out of the
    [i]th statement of [stmts] *)

val eliminate : Lir.t -> Lir.t
(** [eliminate lir] is [lir] with dead definitions removed *)
