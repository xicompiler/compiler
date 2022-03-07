include module type of Lexer

open Core

val string_of_error : string -> error -> string
(** [string_of_error filename e] is the cli error message for the
    lexical error [e] in [filename] *)

(** The [Diagnostic] module cotains functions for generating diagnostic
    lexer output. *)
module Diagnostic : sig
  type nonrec result = (Parser.token, error) result
  (** A [result] is either a token or a lexical error *)

  val string_of_error : error -> string
  (** [string_of_error e] is the diagnostic error message for [e] *)

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

  val to_file : Lexing.lexbuf -> string -> unit
  (** [to_file lexbuf out] lexes [lexbuf] and writes the results to file
      at path [out] *)

  val file_to_file : src:string -> out:string -> unit XiFile.result
  (** [file_to_file ~src ~dst] lexes the file at [src] and writes the
      results to file at path [out]. It yields [Ok ()] on success and
      [Error msg] on failure. *)
end
