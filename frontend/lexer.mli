type position = {
  line : int;
  column : int;
}
(** [position] is the position, consisting of the line and column *)

(** An [error_caise] is either the presence of an invalid character or
    string literal, or an illegal character in the source file *)
type error_cause =
  | InvalidChar
  | InvalidString
  | InvalidSource

type lexical_error = {
  cause : error_cause;
  position : position;
}
(** An [error] describes the cause and position of an error encountered
    during lexing *)

exception LexicalError of lexical_error
(** An [LexicalError] is a lexical error with an associated position
    where the error ocurred in the buffer *)

val read : Lexing.lexbuf -> Parser.token
(** [read lexbuf] consumes the next lexeme in [lexbuf] and returns the
    corresponding token. Raises: [Error] if, on reading [lexbuf], a
    lexical error occurs. *)

val get_position : Lexing.lexbuf -> position
(** [get_position lexbuf] is the current position of [lexbuf] *)
