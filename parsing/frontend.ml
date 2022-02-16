open Parser

type error =
  | LexicalError of Lexer.lexical_error
  | SyntaxError of Lexer.position

let parse lexbuf =
  try Ok (Parser.start Lexer.read lexbuf) with
  | Lexer.LexicalError err -> Error (LexicalError err)
  | _ ->
      let pos = Lexer.get_position lexbuf in
      Error (SyntaxError pos)
