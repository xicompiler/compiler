exception InvalidChar
(** [InvalidChar] indicates an state where an invalid character literal
    has been read from the lexer buffer *)

val read : Lexing.lexbuf -> Parser.token
(** [read lexbuf] the consumes the next lexeme in [lexbuf] and returns
    the corresponding token. Raises: [InvalidChar] if, on reading
    [lexbuf], an invalid character literal is encountered. *)
