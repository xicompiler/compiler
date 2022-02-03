open Parsing.Lexer

let in_file = "tmp/input.txt"
let out_file = "tmp/output.txt"

let () =
  let print_token oc (pos, tok) = 
    let {line;column} : token_position = pos in
    Printf.fprintf oc "%d:%d\n" line column;
    flush stdout; in

  let file_contents =
    let ch = open_in in_file in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s in

  let oc = open_out out_file in begin
    try
      file_contents |> lex_pos_string |> List.iter (print_token oc);
      close_out oc;
    with e ->
      close_out_noerr oc;
      raise e
    end;
    