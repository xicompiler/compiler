module Lexer : module type of LexerAlias
(** A [Lexer] lexes a lexer buffer and outputs a token stream*)

module Parser : module type of ParserAlias
(** A [Parser] parses a program and yields an abstract syntax tree if
    successful, or a syntax or lexical error on failure *)
