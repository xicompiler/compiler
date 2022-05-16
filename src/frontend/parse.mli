open Core
include module type of Parser

module Error : sig
  type syntax_error = string Position.error
  (** A [syntax_error] is an error resulting from an unsuccessful parse. *)

  type t =
    [ `LexicalError of Lex.error
    | `SyntaxError of syntax_error
    ]
  (** A [t] is either a lexical error or a syntax error. Both variants
      carry the position at which they occur. *)

  val to_string : string -> t -> string
  (** [to_string filename e] is the cli error message for the parsing
      error [e] in [filename] *)
end

type error = Error.t
(** [error] is an alias for [Error.t]*)

exception Exn of error
(** [Exn e] is an exception raised during parsing *)

type 'a start = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a
(** [start] is the type of a parsing function that consumes a lexeme and
    returns a token. *)

type nonrec 'a result = ('a, error) result
(** [parse_result] is [Ok ast] or [Error error] and represents the
    result of a parse. *)

val parse : is_rho:bool -> start:'a start -> Lexing.lexbuf -> 'a result
(** [parse lexbuf] is [Ok ast] if [start lexbuf] [ast],
    [Error SyntaxError] if [start lexbuf] raises a syntax error, and
    [Error LexicalError] if [start lexbuf] raises a lexical error. *)

val parse_prog :
  is_rho:bool -> Lexing.lexbuf -> Ast.Undecorated.t result
(** [parse_prog lexbuf] is [Ok ast] if [start lexbuf] [ast],
    [Error SyntaxError] if [start lexbuf] raises a syntax error, and
    [Error LexicalError] if [start lexbuf] raises a lexical error. *)

val parse_source :
  is_rho:bool -> Lexing.lexbuf -> Ast.Undecorated.source result
(** [parse_source lexbuf] is [Ok ast] if [start lexbuf] [ast],
    [Error SyntaxError] if [start lexbuf] raises a syntax error, and
    [Error LexicalError] if [start lexbuf] raises a lexical error. *)

val parse_intf :
  is_rho:bool -> Lexing.lexbuf -> Ast.Undecorated.intf result
(** [parse_intf lexbuf] is [Ok ast] if [start lexbuf] [ast],
    [Error SyntaxError] if [start lexbuf] raises a syntax error, and
    [Error LexicalError] if [start lexbuf] raises a lexical error. *)

val map :
  is_rho:bool ->
  start:'a start ->
  f:('a -> 'b) ->
  Lexing.lexbuf ->
  'b result
(** [map ~start ~f buf] is applies [f] to the AST parsed from [buf]
    using start symbol [start] *)

(** The [Diagnostic] module cotains functions for generating diagnostic
    parsing output. *)
module Diagnostic : sig
  include File.Diagnostic

  module Error : Util.Stringable.S with type t := error
  (** [Error] represents a lexical or syntax diagnostic error *)
end

(** [File] contains functions for parsing files *)
module File : sig
  val parse_intf : string -> Ast.Undecorated.intf result File.result
  (** [parse_intf_file src] is the result of parsing the file located at
      [src], not checking the file extension *)

  type 'a source = Ast.Undecorated.source -> 'a
  (** An ['a source] maps [Ast.Undecorated.source] to ['a] *)

  type 'a intf = Ast.Undecorated.intf -> 'a
  (** An ['a source] maps [Ast.Undecorated.source] to ['a] *)

  val map_ast :
    f:(Ast.Undecorated.t -> 'a) -> string -> 'a result File.Xi.result
  (** [map_ast ~f file] applies [f] to the AST parsed from [file] *)
end
