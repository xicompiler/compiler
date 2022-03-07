open Core
include module type of Parser

module Error : sig
  type syntax_error = string Position.error
  (** A [syntax_error] is an error resulting from an unsuccessful parse. *)

  (** A [t] is either a lexical error or a syntax error. Both variants
      carry the position at which they occur. *)
  type t =
    | LexicalError of Lex.error
    | SyntaxError of syntax_error

  val string_of_cause : string -> string
  (** [string_of_cause e] is the error message corresponding to [e] *)

  val to_string : string -> t -> string
  (** [to_string filename e] is the cli error message for the parsing
      error [e] in [filename] *)
end

type error = Error.t
(** [error] is an alias for [Error.t]*)

type 'a start = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a
(** [start] is the type of a parsing function that consumes a lexeme and
    returns a token. *)

type nonrec 'a result = ('a, error) result
(** [parse_result] is [Ok ast] or [Error error] and represents the
    result of a parse. *)

val parse : start:'a start -> Lexing.lexbuf -> 'a result
(** [parse lexbuf] is [Ok ast] if [start lexbuf] [ast],
    [Error SyntaxError] if [start lexbuf] raises a syntax error, and
    [Error LexicalError] if [start lexbuf] raises a lexical error. *)

val parse_prog : Lexing.lexbuf -> Ast.t result
(** [parse_prog lexbuf] is [Ok ast] if [start lexbuf] [ast],
    [Error SyntaxError] if [start lexbuf] raises a syntax error, and
    [Error LexicalError] if [start lexbuf] raises a lexical error. *)

val parse_source : Lexing.lexbuf -> Ast.Toplevel.source result
(** [parse_source lexbuf] is [Ok ast] if [start lexbuf] [ast],
    [Error SyntaxError] if [start lexbuf] raises a syntax error, and
    [Error LexicalError] if [start lexbuf] raises a lexical error. *)

val parse_intf : Lexing.lexbuf -> Ast.Toplevel.intf result
(** [parse_intf lexbuf] is [Ok ast] if [start lexbuf] [ast],
    [Error SyntaxError] if [start lexbuf] raises a syntax error, and
    [Error LexicalError] if [start lexbuf] raises a lexical error. *)

val bind :
  f:(Ast.t start -> Lexing.lexbuf -> 'a XiFile.result) ->
  string ->
  'a XiFile.result
(** Let [lexbuf] be a lexer buffer created from [file]. Then
    [bind ~f file] is [f Parse.source lexbuf] if [file] is a xi source
    file, [f Parse.intf file] if [file] is a xi intf file, and [Error]
    if [file] does not exist. *)

val map :
  f:(Ast.t start -> Lexing.lexbuf -> 'a) -> string -> 'a XiFile.result
(** Let [lexbuf] be a lexer buffer created from [file]. Then
    [map ~f file] is [Ok (f Parse.start lexbuf)] if [file] is a xi
    source file, [f Parse.intf file] if [file] is a xi intf file, and
    [Error] if [file] does not exist. *)

(** The [Diagnostic] module cotains functions for generating diagnostic
    parsing output. *)
module Diagnostic : sig
  (** [Error] represents a lexical or syntax diagnostic error *)
  module Error : sig
    val to_string : error -> string
    (** [to_string e] is the diagnostic error message for [e] *)
  end

  val to_file : start:Ast.t start -> Lexing.lexbuf -> string -> unit
  (** [to_file ~start lexbuf out] parses lexer buffer [lexbuf] from
      start symbol [start] and writes the diagnostic output to file at
      path [out] *)

  val file_to_file : src:string -> out:string -> unit XiFile.result
  (** [file_to_file ~src ~out] writes parsing diagnostic information to
      a file located at path [out], reading from file at path [src]. It
      yields [Ok ()] on success and [Error e] on failure, where [e] is a
      string describing the failure. *)
end
