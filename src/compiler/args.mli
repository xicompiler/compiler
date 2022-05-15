module Opt : sig
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

  val phases_of_list : string list -> phases
  (** [phases_of_list lst] represents the phases in [lst] *)

  val config : bool -> t -> t
  (** [config disable opt] is the configured optimization-related
      arguments *)
end

type t = {
  files : string list;
  src_dir : string;
  lib_dir : string;
  std_dir : string;
  diag_out_dir : string;
  asm_out_dir : string;
  lex : bool;
  parse : bool;
  typecheck : bool;
  irgen : bool;
  irrun : bool;
  abstract_asm : bool;
  asmrun : bool;
  opt : Opt.t;
  target : string;
}
(** [t] is the type of arguments passed to the compiler *)
