type t = {
  files : string list;
  out_dir : string option;
  src_dir : string option;
  lib_dir : string option;
  lex : bool;
  parse : bool;
  typecheck : bool;
}

let default =
  {
    files = [];
    out_dir = None;
    src_dir = None;
    lib_dir = None;
    lex = false;
    parse = false;
    typecheck = false;
  }
