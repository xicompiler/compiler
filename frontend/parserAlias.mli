include module type of Parser

(** An [error] is either a lexical error or a syntax error. Both
    variants carry the position at which they occur. *)
type error =
  | LexicalError of Lexer.lexical_error
  | SyntaxError of Lexer.position

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

val parse_file : string -> parse_result option
(** [parse_file file] is [Some res] where [res] is the parsed result of
    [file] if [file] is a [.xi] or [.ixi] file, or [None] if [file] is
    not a Xi file. *)

val parse_files : string list -> (unit, string list) result
(** [parse_files files] calls [parse_file] for each file in [files], and
    is [Ok ()] if there are no errors, or [Error errs] on failure. *)

val print_lexical_error : out_channel -> Lexer.lexical_error -> unit
(** [print_lexical_error dst err] prints the lexical error [err] into
    the [dst] out channel. *)

val print_syntax_error : out_channel -> Lexer.position -> unit
(** [print_syntax_error dst err] prints the syntax error [err] into the
    [dst] out channel. *)

(** The [Diagnostic] module cotains functions for generating diagnostic
    parsing output. *)
module Diagnostic : sig
  val parse_to_file : src:string -> dst:string -> unit
  (** [parse_to_file ~src ~dst] parses the file at path [src] and writes
      the results to the file at path [dst], serialized in an
      S-expression. *)
end
