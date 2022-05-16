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
  include File.Diagnostic

  type nonrec result = (Parser.token, error) result
  (** A [result] is either a token or a lexical error *)

  module Error : Util.Stringable.S with type t := error
  (** [Error] represents a lexical error *)

  val lex : is_rho:bool -> Lexing.lexbuf -> result list
  (** [lex_tok buf] consumes all tokens in [buf] and returns them as a
      list. *)

  val lex_string : is_rho:bool -> string -> result list
  (** [lex_string s] consumes all tokens in [s] and returns them as a
      list. *)
end
