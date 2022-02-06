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

type error = {
  cause : error_cause;
  position : position;
}
(** An [error] describes the cause and position of an error encountered
    during lexing *)

exception Error of error
(** An [Error] is a lexical error with an associated position where the
    error ocurred in the buffer *)

type result = (Parser.token, error) Result.t
(** A lexical [result] is either a token or a lexical error *)

val read : Lexing.lexbuf -> Parser.token
(** [read lexbuf] consumes the next lexeme in [lexbuf] and returns the
    corresponding token. Raises: [Error] if, on reading [lexbuf], a
    lexical error occurs. *)

val read_result : Lexing.lexbuf -> result
(** [read lexbuf] consumes the next lexeme in [lexbuf] and returns the
    corresponding token [Ok tok] on success or [Error e] on error. *)

val lex : Lexing.lexbuf -> result list
(** [lex_tok buf] consumes all tokens in [buf] and returns them as a
    list. *)

val lex_string : string -> result list
(** [lex_string s] consumes all tokens in [s] and returns them as a
    list. *)

val lex_file : string -> result list
(** [lex_file s] consumes all tokens in the file at path [s] and returns
    them as a list. *)

val lex_pos :
  Lexing.lexbuf ->
  ((Parser.token, error_cause) Result.t * position) list
(** [lex_tok_pos buf] consumes all tokens in [buf] and returns them as a
    list with their positions. *)

val lex_to_channel : src:in_channel -> dst:out_channel -> unit
(** [lex_to_channel in_file out_file] lexes [src] and writes the results
    to [dst] *)

val lex_to_file : src:string -> dst:string -> unit
(** [lex_to_file in_file out_file] lexes the file at path [src] and
    writes the results to the file at path [dst] *)
