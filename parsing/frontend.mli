(** An [error] is either a lexical error or a syntax error. Both
    variants carry the position at which they occur. *)
type error =
  | LexicalError of Lexer.lexical_error
  | SyntaxError of Lexer.position

type start = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Ast.t
(** [start] is the type of a parsing function that consumes a lexeme and
    returns a token. *)

type parse_result = (Ast.t, error) result

val parse : start:start -> Lexing.lexbuf -> parse_result
(** [parse ~start lexbuf] is [Ok ast] if [start lexbuf] [ast],
    [Error SyntaxError] if [start lexbuf] raises a syntax error, and
    [Error LexicalError] if [start lexbuf] raises a lexical error. *)

val parse_file : string -> parse_result
(** [parse_file file] is [Ok ast] if [file] contains a valid AST
    matching the extension of [file], or [Error err] on failure. *)

val parse_files : string list -> (unit, error list) result
(** [parse_files files] calls [parse_file] for each file in [files], and
    is [Ok ()] if there are no errors, or [Error errs] on failure. *)

val print_lexical_error : Lexer.lexical_error -> out_channel -> unit
(** [print_lexical_error err dst] prints the lexical error [err] into
    the [dst] out channel. *)

val print_syntax_error : Lexer.position -> out_channel -> unit
(** [print_syntax_error err dst] prints the syntax error [err] into the
    [dst] out channel. *)

val print_errors : error list -> unit
(** [print_errors errs] prints [errs] to stdout. *)
