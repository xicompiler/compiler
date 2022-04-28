open Core

module Args : module type of Args
(** [Args] represents the command line arguments to the compiler *)

type nonrec result = (unit, string list) result
(** A [result] is either [Ok ()] or [Error es], where [es] is the list
    of error message detailing the compilation failure. *)

val compile : Args.t -> result
(** [compile args] is the result of invoking the Xi compiler with
    arguments [args] *)
