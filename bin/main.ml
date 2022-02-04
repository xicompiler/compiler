open Parsing.Lexer

let in_file = "tmp/input.txt"

let out_file = "tmp/output.txt"

let empty_char_literal_src = "tmp/empty_char_literal_src.txt"

let empty_char_literal_dst = "tmp/empty_char_literal_dst.txt"

let () =
  lex_to_file ~src:in_file ~dst:out_file;
  lex_to_file ~src:empty_char_literal_src ~dst:empty_char_literal_dst
