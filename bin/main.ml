open Core
open Parsing

exception FileNotFoundError

let usage_msg = "Usage: xic [options] <source files>"

let input_files = ref []

let output_path = ref ""

let src_path = ref ""

let to_lex = ref false

let to_parse = ref false

let display_help = ref false

let flag_count = ref 0

(** [try_iter e] evaluates [e] and is [()] regardless of whether the
    evaluation of [e] raises an exception. *)
let try_iter e =
  try e with
  | _ -> ()

(** [file_path ~path ~file] is [path/file] if path is non-empty and is
    [file] otherwise *)
let file_path ~path ~file =
  if String.is_empty path then file
  else Printf.sprintf "%s/%s" path file

(** [iter_file f \[extension\] input_file] performs function f on a
    given [input_file] to the previously specified (or default root
    directory) path, putting the result in a file with the same prefix
    as [input_file] but with a [ext] extension. *)
let iter_file f ext input_file =
  let output_file = Filename.chop_extension input_file ^ ext in
  let output_file_path =
    file_path ~path:!output_path ~file:output_file
  in
  let output_file_dir = Filename.dirname output_file_path in
  try_iter (Core.Unix.mkdir_p output_file_dir);
  f
    ~src:(file_path ~path:!src_path ~file:input_file)
    ~dst:output_file_path

let incr_flag_count () = incr flag_count

let speclist =
  [
    ( "-D",
      Arg.Tuple [ Arg.Set_string output_path; Arg.Unit incr_flag_count ],
      "Specify where to place generated diagnostic files." );
    ( "-sourcepath",
      Arg.Tuple [ Arg.Set_string src_path; Arg.Unit incr_flag_count ],
      "Specify where to find input source files." );
    ( "--lex",
      Arg.Tuple [ Arg.Set to_lex; Arg.Unit incr_flag_count ],
      "Generate output from lexical analysis." );
    ( "--parse",
      Arg.Tuple [ Arg.Set to_parse; Arg.Unit incr_flag_count ],
      "Generate output from syntactic analysis." );
    ("--help", Arg.Set display_help, "Print a synopsis of options.");
    ("-help", Arg.Set display_help, "Print a synopsis of options.");
  ]

(** [try_get_files ()] attempts to parse a list of files from command
    line arguments, mutating [input_files] to reflect the parsed files. *)
let try_get_files () =
  let file_acc f = input_files := f :: !input_files in
  try Arg.parse speclist file_acc usage_msg with
  | _ -> print_endline (Arg.usage_string speclist usage_msg)

let parse_command () =
  try_get_files ();
  if !display_help || !flag_count = 0 then
    print_endline (Arg.usage_string speclist usage_msg)

(** [try_sys_iter f \[e1; ...; en\]] is [f e1; ...; f en]. If
    [Sys_error] is raised during evaluation, the error message is
    printed to stdout. *)
let try_sys_iter ~f lst =
  try List.iter ~f lst with
  | Sys_error err -> print_endline err

(** [lex_diagonistic ()] writes the lexer output to a diagnostic file. *)
let lex_diagnostic () =
  try_sys_iter
    ~f:(iter_file LexerDebug.lex_to_file ".lexed")
    !input_files

(** [parse_diagonistic ()] writes the parser output to a diagnostic
    file. *)
let parse_diagnostic () =
  try_sys_iter
    ~f:(iter_file ParserDebug.parse_to_file ".parsed")
    !input_files

let compile () = Frontend.parse_files !input_files

let () =
  parse_command ();
  if !to_lex then lex_diagnostic ();
  if !to_parse then parse_diagnostic ();
  if !to_lex || !to_parse then exit 0;
  match compile () with
  | Ok () -> exit 0
  | Error errors ->
      List.iter ~f:print_endline errors;
      exit 1
