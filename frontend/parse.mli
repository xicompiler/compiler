open Core
include module type of Parser

(** An [error] is either a lexical error or a syntax error. Both
    variants carry the position at which they occur. *)
type error =
  | LexicalError of Lex.error
  | SyntaxError of Lex.position

type start = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Ast.t
(** [start] is the type of a parsing function that consumes a lexeme and
    returns a token. *)

type nonrec result = (Ast.t, error) result
(** [parse_result] is [Ok ast] or [Error error] and represents the
    result of a parse. *)

val parse : start:start -> Lexing.lexbuf -> result
(** [parse ~start lexbuf] is [Ok ast] if [start lexbuf] [ast],
    [Error SyntaxError] if [start lexbuf] raises a syntax error, and
    [Error LexicalError] if [start lexbuf] raises a lexical error. *)

val string_of_error : error -> string
(** [string_of_error e] is the string representing error [e] *)

val bind :
  f:(start -> Lexing.lexbuf -> 'a XiFile.result) ->
  string ->
  'a XiFile.result
(** Let [lexbuf] be a lexer buffer created from [file]. Then
    [bind ~f file] is [f Parse.start lexbuf] if [file] is a xi source
    file, [f Parse.interface file] if [file] is a xi interface file, and
    [Error] if [file] does not exist. *)

val map : f:(start -> Lexing.lexbuf -> 'a) -> string -> 'a XiFile.result
(** Let [lexbuf] be a lexer buffer created from [file]. Then
    [map ~f file] is [Ok (f Parse.start lexbuf)] if [file] is a xi
    source file, [f Parse.interface file] if [file] is a xi interface
    file, and [Error] if [file] does not exist. *)

(** The [Diagnostic] module cotains functions for generating diagnostic
    parsing output. *)
module Diagnostic : sig
  val to_file : start:start -> Lexing.lexbuf -> string -> unit
  (** [to_file ~start lexbuf out] parses lexer buffer [lexbuf] from
      start symbol [start] and writes the diagnostic output to file at
      path [out] *)

  val file_to_file : src:string -> out:string -> unit XiFile.result
  (** [file_to_file ~src ~out] writes parsing diagnostic information to
      a file located at path [out], reading from file at path [src]. It
      yields [Ok ()] on success and [Error e] on failure, where [e] is a
      string describing the failure. *)
end
