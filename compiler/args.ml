type t = {
  files : string list;
  out_dir : string option;
  src_dir : string option;
  lib_dir : string option;
  lex : bool;
  parse : bool;
  typecheck : bool;
}
