open Core

val ok_if_true_lazy : error:(unit -> 'a) -> bool -> (unit, 'a) result
(** [ok_if_true_lazy ~error b] is [Ok ()] if [b] is [true] and
    [Error (error ())] if [b] is false *)
