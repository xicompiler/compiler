type phases = {
  initial : bool;
  final : bool;
}
(** [phases] is the type of optimization phases *)

type t = {
  optir : phases;
  optcfg : phases;
  cf : bool; (* constant folding *)
  reg : bool; (* register allocation *)
  copy : bool; (* copy propagation *)
  dce : bool; (* dead code elimination *)
  cp : bool; (* constant propagation *)
  vn : bool; (* local value numbering *)
}
(** [opt] is the type of optimization-related arguments passed to the
    compiler *)

val enabled : t -> t
(** [enabled opt] is the optimization-related arguments if optimizations
    are enabled *)

val disabled : t
(** [disabled] is the initial optimization-related arguments *)
