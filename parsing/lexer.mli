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

val lex_tok : Lexing.lexbuf -> Parser.token list
(** [lex_tok buf] consumes all tokens in [buf] and returns them as a
    list. *)

val lex_tok_string : string -> Parser.token list
(** [lex_tok_string s] consumes all tokens in [s] and returns them as a
    list. *)

val lex_pos_tok : Lexing.lexbuf -> (position * Parser.token) list
(** [lex_tok_pos buf] consumes all tokens in [buf] and returns them as a
    list with their positions. *)

val lex_pos_tok_string : string -> (position * Parser.token) list
(** [lex_tok_pos_string s] consumes all tokens in [s] and returns them
    as a list with their positions. *)

val lex : string -> string -> unit
(** [lex in_file out_file] lexes [in_file] and writes the output to
    [out_file] *)
