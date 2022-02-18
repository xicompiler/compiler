include module type of Parser

(** An [error] is either a lexical error or a syntax error. Both
    variants carry the position at which they occur. *)
type error =
  | LexicalError of Lex.lexical_error
  | SyntaxError of Lex.position

type start = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Ast.t
(** [start] is the type of a parsing function that consumes a lexeme and
    returns a token. *)

type parse_result = (Ast.t, error) result
(** [parse_result] is [Ok ast] or [Error error] and represents the
    result of a parse. *)

val parse : start:start -> Lexing.lexbuf -> parse_result
(** [parse ~start lexbuf] is [Ok ast] if [start lexbuf] [ast],
    [Error SyntaxError] if [start lexbuf] raises a syntax error, and
    [Error LexicalError] if [start lexbuf] raises a lexical error. *)

val parse_files : string list -> (unit, string list) result
(** [parse_files files] calls [parse_file] for each file in [files], and
    is [Ok ()] if there are no errors, or [Error errs] on failure. *)

(** The [Diagnostic] module cotains functions for generating diagnostic
    parsing output. *)
module Diagnostic : sig
  val parse_to_file : src:string -> dst:string -> unit
  (** [parse_to_file ~src ~dst] parses the file at path [src] and writes
      the results to the file at path [dst], serialized in an
      S-expression. *)
end
