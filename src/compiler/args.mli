val phases_of_list : string list -> Opt.phases
(** [phases_of_list lst] represents the phases in [lst] *)

val config : bool -> Opt.t -> Opt.t
(** [config disable opt] is the configured optimization-related
    arguments *)

val opts : string list
(** [opts] is the list of supported optimizations *)

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
