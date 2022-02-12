open Parsing.Lexer
open Parsing.ParserDebug

exception FileNotFoundError

let usage_msg = "Usage: xic [options] <source files>"

let input_files = ref []

let output_path = ref ""

let src_path = ref ""

let to_lex = ref false

let to_parse = ref false

let display_help = ref false

let flag_count = ref 0

(** [op_on_file f [extension] input_file] performs function f on a given [input_file] to the
    previously specified (or default root directory) path, putting the
    result in a file with the same prefix as [input_file] but with a
    [ext] extension. *)
let op_on_file f ext input_file =
  let file_ext = Filename.extension input_file in 
  if file_ext = ".xi" || file_ext = ".ixi"  then
    let file_prefix = 
      input_file |> Filename.remove_extension
    in
      let output_file_path =
        if !output_path = "" then file_prefix ^ ext
        else !output_path ^ "/" ^ file_prefix ^ ext
      in
      let output_file_dir = Filename.dirname output_file_path in
      (try Core.Unix.mkdir_p output_file_dir with
      | _ -> ());
      f ~src:(if !src_path = "" then input_file 
        else !src_path ^ "/" ^ input_file)
        ~dst:output_file_path
  else
    print_endline "non .xi or .ixi file passed in - ignored"

let inc_flag_count () = 
  flag_count := !flag_count + 1

let speclist =
  [
    ( "-D",
      Arg.Tuple [ Arg.Set_string output_path; Arg.Unit inc_flag_count ],
      "Specify where to place generated diagnostic files." );
    ( "-sourcepath",
      Arg.Tuple [ Arg.Set_string src_path; Arg.Unit inc_flag_count ],
      "Specify where to find input source files." );
    ( "--lex",
      Arg.Tuple [ Arg.Set to_lex; Arg.Unit inc_flag_count ],
      "Generate output from lexical analysis." );
    ( "--parse",
      Arg.Tuple [ Arg.Set to_parse; Arg.Unit inc_flag_count ],
      "Generate output from syntactic analysis." );
    ("--help", Arg.Set display_help, "Print a synopsis of options.");
    ("-help", Arg.Set display_help, "Print a synopsis of options.");
  ]

let () =
  let _ =
    try
      Arg.parse speclist
        (fun f -> input_files := f :: !input_files)
        usage_msg
    with
    | _ -> print_endline (Arg.usage_string speclist usage_msg)
  in
  if !display_help || !flag_count = 0 then
    print_endline (Arg.usage_string speclist usage_msg);
  if !to_lex then
    try List.iter (op_on_file lex_to_file ".lexed") !input_files with
    | Sys_error err -> print_endline err;
  if !to_parse then
    try List.iter (op_on_file parse_to_file ".parsed") !input_files with
    | Sys_error err -> print_endline err