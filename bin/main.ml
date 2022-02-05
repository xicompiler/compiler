open Parsing.Lexer

let usage_msg = "Usage: xic [options] <source files>"

let input_files = ref []

let output_path = ref ""

let to_lex = ref false

let get_file_prefix filename = List.hd (String.split_on_char '.' filename)

let lex_file_to_path input_file =
  let file_prefix = get_file_prefix input_file in
  let output_file_path = !output_path ^ "/" ^ file_prefix ^ ".lexed" in
  try
    Core.Unix.mkdir_p !output_path;
  with
    | _ -> ();
  print_endline "here";
  Parsing.Lexer.lex_to_file input_file output_file_path

let lex_files intput_files = 
  List.iter lex_file_to_path !input_files

let speclist =
  [("-D", Arg.Set_string output_path, "Specify where to place generated diagnostic files.");
   ("--lex", Arg.Set to_lex, "Generate output from lexical analysis.")]

let () = 
  let _ = 
    Arg.parse speclist (fun f -> input_files := f :: !input_files) usage_msg in
  if !to_lex then lex_files ()
