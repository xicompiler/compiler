open Core
include module type of Lexer

(** [Error] represents a lexical error *)
module Error : sig
  include module type of struct
    include Error
  end

  val to_string : string -> t -> string
  (** [to_string filename e] is the cli error message for the lexical
      error [e] in [filename] *)
end

(** The [Diagnostic] module cotains functions for generating diagnostic
    lexer output. *)
module Diagnostic : sig
  type nonrec result = (Parser.token, error) result
  (** A [result] is either a token or a lexical error *)

  module Error : sig
    val to_string : error -> string
    (** [to_string e] is the error message corresponding to [e]
        containing both its cause and position *)
  end

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
