open Lexer

type result = (Parser.token, lexical_error) Result.t
(** A lexical [result] is either a token or a lexical error *)

val read_result : Lexing.lexbuf -> result
(** [read lexbuf] consumes the next lexeme in [lexbuf] and returns the
    corresponding token [Ok tok] on success or [LexicalError e] on
    error. *)

val lex : Lexing.lexbuf -> result list
(** [lex_tok buf] consumes all tokens in [buf] and returns them as a
    list. *)

val lex_string : string -> result list
(** [lex_string s] consumes all tokens in [s] and returns them as a
    list. *)

val lex_pos :
  Lexing.lexbuf ->
  ((Parser.token, error_cause) Result.t * position) list
(** [lex_tok_pos buf] consumes all tokens in [buf] and returns them as a
    list with their positions. *)

val lex_to_channel : src:in_channel -> dst:out_channel -> unit
(** [lex_to_channel in_file out_file] lexes [src] and writes the results
    to [dst] *)

val lex_to_file : src:string -> dst:string -> unit
(** [lex_to_file ~src ~dst] lexes the file at path [src] and writes the
    results to the file at path [dst] *)
