type token_position = {
  line : int;
  column : int;
}
(** [token_position] is the position, consisting of the line and column *)

exception InvalidChar of token_position
(** [InvalidChar] indicates an state where an invalid character literal
    has been read from the lexer buffer *)

exception InvalidString of token_position
(** [InvalidString] indicates an state where an invalid string literal
    has been read from the lexer buffer *)

val read : Lexing.lexbuf -> Parser.token
(** [read lexbuf] consumes the next lexeme in [lexbuf] and returns the
    corresponding token. Raises: [InvalidChar] if, on reading [lexbuf],
    an invalid character literal is encountered. *)

val lex : Lexing.lexbuf -> Parser.token list
(** [lex buf] consumes all tokens in [buf] and returns them as a list.
    Requires: [buf] has no lexical errors *)

val lex_string : string -> Parser.token list
(** [lex_string s] consumes all tokens in [s] and returns them as a
    list. Requires: [s] has no lexical errors *)

val lex_pos : Lexing.lexbuf -> (token_position * Parser.token) list
(** [lex buf] consumes all tokens in [buf] and returns them as a list.
    Requires: [buf] has no lexical errors *)

val lex_pos_string : string -> (token_position * Parser.token) list
(** [lex_string s] consumes all tokens in [s] and returns them as a
    list. Requires: [s] has no lexical errors *)
