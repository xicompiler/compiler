type t = {
  files : string list;
  out_dir : string;
  src_dir : string;
  lib_dir : string;
  std_dir : string;
  lex : bool;
  parse : bool;
  typecheck : bool;
  irgen : bool;
  irrun : bool;
  optimize : bool;
}
