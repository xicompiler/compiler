type position = {
  line : int;
  column : int;
}
(** [position] is the position, consisting of the line and column *)

(** A [lexical_error] is either due to the presence of an invalid
    character or string literal, or an illegal character in the source
    file *)
type lexical_error =
  | InvalidChar
  | InvalidString
  | InvalidSource

exception
  Error of {
    cause : lexical_error;
    position : position;
  }
(** An [Error] is a lexical error with an associated position where the
    error ocurred in the buffer *)

val read : Lexing.lexbuf -> Parser.token
(** [read lexbuf] consumes the next lexeme in [lexbuf] and returns the
    corresponding token. Raises: [InvalidChar] if, on reading [lexbuf],
    an invalid character literal is encountered. *)

val lex : Lexing.lexbuf -> Parser.token list
(** [lex buf] consumes all tokens in [buf], excluding [EOF] and returns
    them as a list. Requires: [buf] has no lexical errors *)

val lex_string : string -> Parser.token list
(** [lex_string s] consumes all tokens in [s] and returns them as a
    list. Requires: [s] has no lexical errors *)

val lex_pos : Lexing.lexbuf -> (position * Parser.token) list
(** [lex buf] consumes all tokens in [buf] and returns them as a list.
    Requires: [buf] has no lexical errors *)

val lex_pos_string : string -> (position * Parser.token) list
(** [lex_string s] consumes all tokens in [s] and returns them as a
    list. Requires: [s] has no lexical errors *)
