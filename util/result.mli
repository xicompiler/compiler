open Core

val ok_if_true_lazy : error:(unit -> 'a) -> bool -> (unit, 'a) result
(** [ok_if_true_lazy ~error b] is [Ok ()] if [b] is [true] and
    [Error (error ())] if [b] is false *)

val ( >>? ) : ('a, 'b) result -> ('b -> 'c) -> ('a, 'c) result
(** [r >>? f] is [Core.Result.map_error ~f r] *)
