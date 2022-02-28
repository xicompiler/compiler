(** An [error_caise] is either the presence of an invalid character or
    string literal, or an illegal character in the source file *)
type error_cause =
  | InvalidChar
  | InvalidString
  | InvalidSource

type error = {
  cause : error_cause;
  position : Position.t;
}
(** An [error] describes the cause and position of an error encountered
    during lexing *)

exception Error of error
(** An [Error] is a lexical error with an associated position where the
    error ocurred in the buffer *)

val read : Lexing.lexbuf -> Parser.token
(** [read lexbuf] consumes the next lexeme in [lexbuf] and returns the
    corresponding token. Raises: [Error] if, on reading [lexbuf], a
    lexical error occurs. *)
