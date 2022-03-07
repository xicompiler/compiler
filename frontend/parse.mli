open Core

include module type of Parser

type syntax_error = string Position.error
(** A [syntax_error] is an error resulting from an unsuccessful parse. *)

(** An [error] is either a lexical error or a syntax error. Both
    variants carry the position at which they occur. *)
type error =
  | LexicalError of Lex.error
  | SyntaxError of syntax_error

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

val string_of_error : string -> error -> string
(** [string_of_error filename e] is the cli error message for the
    parsing error [e] in [filename] *)

val bind :
  f:(Ast.t start -> Lexing.lexbuf -> 'a XiFile.result) ->
  string ->
  'a XiFile.result
(** Let [lexbuf] be a lexer buffer created from [file]. Then
    [bind ~f file] is [f Parse.start lexbuf] if [file] is a xi source
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
  val string_of_error : error -> string
  (** [string_of_error e] is the string representing error [e] *)

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
