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
  disable_optimize : bool;
  target : string;
}
