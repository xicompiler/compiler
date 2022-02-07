open Parsing.Lexer

exception FileNotFoundError
let usage_msg = "Usage: xic [options] <source files>"

let input_files = ref []

let output_path = ref ""

let to_lex = ref false

let display_help = ref true

let get_file_prefix filename =
  List.hd (String.split_on_char '.' filename)

(** [get_file_path filename] gets the output directory path to lex to given full 
  path [filename]. *)
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

(** [lex_file_to_path input_file] lexes a given [input_file] to the previously specified
 (or default root directory) path, putting the result in a file with the same
 prefix as [input_file] but with a .lexed extension. *)
let lex_file_to_path input_file =
  if Filename.extension input_file = ".xi" then
    let file_prefix = 
      input_file |> Filename.remove_extension
    in
      let output_file_path =
        if !output_path = "" then file_prefix ^ ".lexed"
        else !output_path ^ "/" ^ file_prefix ^ ".lexed"
      in
      let output_file_dir = get_file_path output_file_path in
      (try Core.Unix.mkdir_p output_file_dir with
      | _ -> ());
      Parsing.Lexer.lex_to_file ~src:input_file ~dst:output_file_path
  else
    print_endline "non .xi file passed in - ignored"; ()

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
    | _ -> print_endline (Arg.usage_string speclist usage_msg) in
  if !display_help then print_endline (Arg.usage_string speclist usage_msg);
  if !to_lex then 
    try List.iter lex_file_to_path !input_files with 
    | Sys_error err -> print_endline err