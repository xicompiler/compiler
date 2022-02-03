exception LexicalError
(** [LexicalError] indicates a lexical error where the input symbol is
    not part of the Xi language *)

type token_position = {
  line : int;
  column : int;
}
(** [token_position] is the position, consisting of the line and column *)

exception InvalidChar of token_position
(** [InvalidChar] indicates an state where an invalid character literal
    has been read from the lexer buffer *)

exception InvalidString of token_position
(** [InvalidString] indicates an state where an invalid string literal
    has been read from the lexer buffer *)

val read : Lexing.lexbuf -> Parser.token
(** [read lexbuf] consumes the next lexeme in [lexbuf] and returns the
    corresponding token. Raises: [InvalidChar] if, on reading [lexbuf],
    an invalid character literal is encountered. *)

val lex_tok : Lexing.lexbuf -> Parser.token list
(** [lex_tok buf] consumes all tokens in [buf] and returns them as a
    list. *)

val lex_tok_string : string -> Parser.token list
(** [lex_tok_string s] consumes all tokens in [s] and returns them as a
    list. *)

val lex_pos_tok : Lexing.lexbuf -> (token_position * Parser.token) list
(** [lex_tok_pos buf] consumes all tokens in [buf] and returns them as a
    list with their positions. *)

val lex_pos_tok_string : string -> (token_position * Parser.token) list
(** [lex_tok_pos_string s] consumes all tokens in [s] and returns them
    as a list with their positions. *)

val lex : string -> string -> unit
(** [lex in_file out_file] lexes [in_file] and writes the output to
    [out_file] *)
