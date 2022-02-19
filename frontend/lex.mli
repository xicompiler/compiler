include module type of Lexer

open Core

val string_of_error : lexical_error -> string
(** [string_of_error_cause e] is the error message corresponding to [e]
    containing both its cause and poisiton *)

val format_position : position -> string -> string
(** [format_position pos s] is the message [line:col s] followed by a
    newline where [line] and [col] are described by [pos] *)

(** The [Diagnostic] module cotains functions for generating diagnostic
    lexer output. *)
module Diagnostic : sig
  type lex_result = (Parser.token, lexical_error) result
  (** A [lex_result] is either a token or a lexical error *)

  val read_result : Lexing.lexbuf -> lex_result
  (** [read lexbuf] consumes the next lexeme in [lexbuf] and returns the
      corresponding token [Ok tok] on success or [LexicalError e] on
      error. *)

  val lex : Lexing.lexbuf -> lex_result list
  (** [lex_tok buf] consumes all tokens in [buf] and returns them as a
      list. *)

  val lex_string : string -> lex_result list
  (** [lex_string s] consumes all tokens in [s] and returns them as a
      list. *)

  val print_position : Out_channel.t -> Lexer.position -> string -> unit
  (** [print_position out pos s] prints the message [line:col s] to
      [out] followed by a newline where [line] and [col] are described
      by [pos] *)

  val print_error : Out_channel.t -> lexical_error -> unit
  (** [print_error dst err] prints an error message detailing the
      position and cause of [err] to out channel [out]*)

  val lex_to_channel : In_channel.t -> Out_channel.t -> unit
  (** [lex_to_channel in_file out_file] lexes [src] and writes the
      results to [dst] *)

  val lex_to_file : src:string -> dst:string -> unit
  (** [lex_to_file ~src ~dst] lexes the file at path [src] and writes
      the results to the file at path [dst] *)
end
