open Core

type args = {
  files : string list;
  out_dir : string option;
  src_dir : string option;
  lex : bool;
  parse : bool;
  help : bool;
}
(** [args] is the type of command line arguments to the Xi compiler *)

let usage_msg = "Usage: xic [options] <source files>"

let files = ref []

let out_dir = ref ""

let src_dir = ref ""

let lex = ref false

let parse = ref false

let help = ref false

let speclist =
  [
    ( "-D",
      Arg.Set_string out_dir,
      "Specify where to place generated diagnostic files." );
    ( "-sourcepath",
      Arg.Set_string src_dir,
      "Specify where to find input source files." );
    ("--lex", Arg.Set lex, "Generate output from lexical analysis.");
    ( "--parse",
      Arg.Set parse,
      "Generate output from syntactic analysis." );
    ("--help", Arg.Set help, "Print a synopsis of options.");
    ("-help", Arg.Set help, "Print a synopsis of options.");
  ]

(** [print_help ()] prints the help message. *)
let print_help () = print_string (Arg.usage_string speclist usage_msg)

(** [none_if_empty s] is [None] if [s] is [""] and [Some s] otherwise. *)
let none_if_empty s = if String.is_empty s then None else Some s

(** [parse_args ()] is [Some args] if [args] were succesfully parsed
    from the command line and [None] otherwise. *)
let parse_args () =
  let file_acc f = files := f :: !files in
  Option.try_with (fun () ->
      Arg.parse speclist file_acc usage_msg;
      {
        files = !files;
        out_dir = none_if_empty !out_dir;
        src_dir = none_if_empty !src_dir;
        lex = !lex;
        parse = !parse;
        help = !help;
      })

(** [try_compile args] attempts to compile a program described by
    arguments [args], exiting with code 1 on error. *)
let try_compile { files; out_dir; src_dir; lex; parse; help } =
  if List.is_empty files || help then print_help ();
  let iter_errors es =
    List.iter ~f:print_endline es;
    if not (lex || parse) then exit 1
  in
  files
  |> Compile.compile ?src_dir ?out_dir ~lex ~parse
  |> Result.iter_error ~f:iter_errors

let () =
  match parse_args () with
  | Some args -> try_compile args
  | None -> print_help ()
