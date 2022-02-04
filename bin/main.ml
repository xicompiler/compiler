open Parsing.Lexer

let in_file = "tmp/input.txt"

let out_file = "tmp/output.txt"

let () = lex_to_file ~src:in_file ~dst:out_file
