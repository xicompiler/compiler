(** An [error] is either a lexical error or a syntax error. Both
    variants carry the position at which they occur. *)
type error =
  | LexicalError of Lexer.lexical_error
  | SyntaxError of Lexer.position

val parse : Lexing.lexbuf -> (Ast.t, error) result
(** [parse lexbuf] is the AST representing the program contained withing
    lexer buffer [lexbuf]. Raises: [LexicalError] if [lexbuf] contains a
    lexical error, or [SyntaxError] the sequence of tokens lexed from
    [lexbuf] is invalid*)
