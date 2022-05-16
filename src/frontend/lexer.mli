(** [Error] represents a lexical error *)
module Error : sig
  (** An [error_caise] is either the presence of an invalid character or
      string literal, or an illegal character in the source file *)
  type cause =
    | InvalidChar
    | InvalidString
    | InvalidSource

  type t = cause Position.error
  (** An [t] describes the cause and position of an error encountered
      during lexing *)
end

type error = Error.t
(** [error] is an alias for [Error.t]*)

exception Error of error
(** An [Error] is a lexical error with an associated position where the
    error ocurred in the buffer *)

val read_rho : Lexing.lexbuf -> Parser.token
(** [read_rho lexbuf] consumes the next lexeme in [lexbuf] and returns
    the corresponding token for the Rho language. Raises: [Error] if, on
    reading [lexbuf], a lexical error occurs. *)

val read_xi : Lexing.lexbuf -> Parser.token
(** [read_xi lexbuf] consumes the next lexeme in [lexbuf] and returns
    the corresponding token for the Xi language. Raises: [Error] if, on
    reading [lexbuf], a lexical error occurs. *)
