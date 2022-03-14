include Abstract.S

val position : 'a t -> Position.t
(** [position node] is the position wrapped by node [node] *)

val make : pos:Position.t -> 'a -> 'a t
(** [make ~pos value] is a [Position] carrying value [value] and
    position [position] *)

val error : cause:'err -> 'a t -> 'err Position.error
(** [error ~cause pos] is a position with cause [cause] occurring at
    position [pos] *)
