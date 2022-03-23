open Core
open Parser
open Position
include Lexer

module Error = struct
  include Error

  (** [desc cause] is the description of error cause [cause] *)
  let desc = function
    | InvalidChar -> "Invalid character constant"
    | InvalidString -> "Invalid string constant"
    | InvalidSource -> "Illegal character in source file"

  (** [string_of_cause e] is the error message corresponding to [e] *)
  let string_of_cause cause = cause |> desc |> Printf.sprintf "error:%s"

  (** [fmt] is the format for a lexical error message *)
  let fmt = format_of_string "Lexical error beginning at %s:%s"

  let to_string filename error =
    Position.Error.format_last ~f:desc ~msg:filename ~fmt error
end

module Diagnostic = struct
  type nonrec result = (Parser.token, error) result

  module Error = struct
    let to_string = Position.Error.format ~f:Error.string_of_cause
  end

  (** [string_of_char_token c] is the string representing char token [c] *)
  let string_of_char_token u =
    u |> Unicode.to_string |> Printf.sprintf "character %s"

  (** [string_of_string_token s] is the string representing string token
      [s] *)
  let string_of_string_token s =
    s |> Unicode.escape_string |> Printf.sprintf "string %s"

  (** [string_of_int_token i] is the string representing int token [i] *)
  let string_of_int_token = Printf.sprintf "integer %s"

  (** [string_of_bool_token b] is the string representing bool token [b] *)
  let string_of_bool_token = Bool.to_string

  (** [string_of_id_token x] is the string representing id token [x] *)
  let string_of_id_token = Printf.sprintf "id %s"

  (** [string_of_token tok] is the string representation of token [tok] *)
  let string_of_token = function
    | USE -> "use"
    | IF -> "if"
    | ELSE -> "else"
    | WHILE -> "while"
    | RETURN -> "return"
    | LENGTH -> "length"
    | CHAR u -> string_of_char_token u
    | STRING s -> string_of_string_token s
    | INT i -> string_of_int_token i
    | BOOL b -> string_of_bool_token b
    | LPAREN -> "("
    | RPAREN -> ")"
    | LBRACKET -> "["
    | RBRACKET -> "]"
    | LBRACE -> "{"
    | RBRACE -> "}"
    | GETS -> "="
    | MULT -> "*"
    | HIGHMULT -> "*>>"
    | DIV -> "/"
    | MOD -> "%"
    | PLUS -> "+"
    | MINUS -> "-"
    | LT -> "<"
    | LEQ -> "<="
    | GEQ -> ">="
    | GT -> ">"
    | EQ -> "=="
    | NEQ -> "!="
    | NOT -> "!"
    | AND -> "&"
    | OR -> "|"
    | COLON -> ":"
    | SEMICOLON -> ";"
    | COMMA -> ","
    | ID x -> string_of_id_token x
    | WILDCARD -> "_"
    | EOF -> "EOF"
    | TYPE p -> Type.Tau.Primitive.to_string p

  (** [read_result lexbuf] consumes the next lexeme in [lexbuf] and
      returns the corresponding token [Ok tok] on success or
      [LexicalError e] on error. *)
  let read_result lexbuf =
    try Ok (read lexbuf) with Error e -> Result.Error e

  (** [lex_pos_rev lexbuf] is a reversed list of [(result, position)]
      pairs of all tokens lexed from lexbuf *)
  let lex_pos_rev lexbuf =
    let rec help acc =
      let res = read_result lexbuf in
      let pos = Position.get_position_lb lexbuf in
      match res with
      | Ok EOF -> acc
      | Ok _ -> help ((res, pos) :: acc)
      | Error _ -> (res, pos) :: acc
    in
    help []

  (** [lex_tok_pos buf] consumes all tokens in [buf] and returns them as
      a list with their positions. *)
  let lex_pos lexbuf = lexbuf |> lex_pos_rev |> List.rev

  let lex lexbuf = lexbuf |> lex_pos_rev |> List.rev_map ~f:fst
  let lex_string s = s |> Lexing.from_string |> lex

  (** [print_result out] prints the valid token or an error message into
      the [out] out channel. *)
  let print_result out res =
    let s =
      match res with
      | Ok tok, pos -> tok |> string_of_token |> Position.format pos
      | Error e, _ -> Error.to_string e
    in
    Printf.fprintf out "%s\n" s

  (** [to_channel lexbuf out] lexes [lexbuf] and writes the results to
      [out] *)
  let to_channel lexbuf out =
    lexbuf |> lex_pos |> List.iter ~f:(print_result out)

  (** [to_file lexbuf out] lexes [lexbuf] and writes the results to file
      at path [out] *)
  let to_file lexbuf out =
    Out_channel.with_file ~f:(to_channel lexbuf) out

  let file_to_file ~src ~out =
    File.Xi.map_same_fn ~f:(fun buf -> to_file buf out) src
end