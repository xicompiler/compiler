include module type of Lexer

val string_of_error : lexical_error -> string
(** [string_of_error_cause e] is the error message corresponding to [e]
    containing both its cause and poisiton *)

val format_error : position -> string -> string
(** [string_of_error pos s] is the error message [line:col s] to [out]
    followed by a newline where [line] and [col] are described by [pos] *)

(** The [Diagnostic] module cotains functions for generating diagnostic
    lexer output. *)
module Diagnostic : sig
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

  val print_pos : out_channel -> Lexer.position -> string -> unit
  (** [print_pos out pos s] prints the message [line:col s] to [out]
      followed by a newline where [line] and [col] are described by
      [pos] *)

  val print_error : out_channel -> lexical_error -> unit
  (** [print_error dst err] prints an error message detailing the
      position and cause of [err] to out channel [out]*)

  val lex_to_channel : src:in_channel -> dst:out_channel -> unit
  (** [lex_to_channel in_file out_file] lexes [src] and writes the
      results to [dst] *)

  val lex_to_file : src:string -> dst:string -> unit
  (** [lex_to_file ~src ~dst] lexes the file at path [src] and writes
      the results to the file at path [dst] *)
end
