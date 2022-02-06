open Parsing.Lexer

let usage_msg = "Usage: xic [options] <source files>"

let input_files = ref []

let output_path = ref ""

let to_lex = ref false

let display_help = ref true

let get_file_prefix filename =
  List.hd (String.split_on_char '.' filename)

let get_file_path filename =
  let f = String.split_on_char '/' filename in
  let rec get_first_part path acc =
    match path with
    | [] -> acc
    | [ h ] -> acc
    | h :: t ->
        if acc = "" then get_first_part t h
        else get_first_part t (acc ^ "/" ^ h)
  in
  get_first_part f ""

let lex_file_to_path input_file =
  let file_prefix = get_file_prefix input_file in
  let output_file_path =
    if !output_path = "" then file_prefix ^ ".lexed"
    else !output_path ^ "/" ^ file_prefix ^ ".lexed"
  in
  let output_file_dir = get_file_path output_file_path in
  (try Core.Unix.mkdir_p output_file_dir with
  | _ -> ());
  Parsing.Lexer.lex_to_file ~src:input_file ~dst:output_file_path

let lex_files intput_files = List.iter lex_file_to_path !input_files

let speclist =
  [
    ( "-D",
      Arg.Tuple [ Arg.Set_string output_path; Arg.Clear display_help ],
      "Specify where to place generated diagnostic files." );
    ( "--lex",
      Arg.Tuple [ Arg.Set to_lex; Arg.Clear display_help ],
      "Generate output from lexical analysis." );
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
  if !display_help then
    print_endline (Arg.usage_string speclist usage_msg);
  if !to_lex then lex_files ()
