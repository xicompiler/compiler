type t = {
  files : string list;
  out_dir : string option;
  src_dir : string option;
  lex : bool;
  parse : bool;
  type_check : bool;
  help : bool;
}

let default =
  {
    files = [];
    out_dir = None;
    src_dir = None;
    lex = false;
    parse = false;
    type_check = false;
    help = false;
  }
