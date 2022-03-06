type t = {
  files : string list;
  out_dir : string option;
  src_dir : string option;
  lex : bool;
  parse : bool;
  type_check : bool;
  help : bool;
}
(** [t] is the type of arguments passed to the compiler *)

val default : t
(** [default] contains the default command line arguments to the
    compiler *)
