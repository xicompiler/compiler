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
  disable_optimize : bool;
  target : string;
}
(** [t] is the type of arguments passed to the compiler *)
