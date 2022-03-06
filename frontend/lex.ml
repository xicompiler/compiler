open Core
open Parser
open Position
include Lexer

let error_description = function
  | InvalidChar -> "Invalid character constant"
  | InvalidString -> "Invalid string constant"
  | InvalidSource -> "Illegal character in source file"

(** [string_of_error_cause e] is the error message corresponding to [e] *)
let string_of_error_cause cause =
  cause |> error_description |> Printf.sprintf "error:%s"

let format_position { line; column } =
  Printf.sprintf "%d:%d %s" line column

let string_of_error error =
  let pos = Error.position error in
  error |> Error.cause |> string_of_error_cause |> format_position pos

let cli_string_of_error filename error =
  let { line; column } = Error.position error in
  error |> Error.cause |> string_of_error_cause
  |> Printf.sprintf "Lexical error beginning at %s:%d:%d: %s" filename
       line column

module Diagnostic = struct
  type nonrec result = (Parser.token, error) result

  (** [string_of_char_token c] is the string representing char token [c] *)
  let string_of_char_token u =
    u |> Unicode.string_of_uchar |> Printf.sprintf "character %s"

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
    | TYPE `Int -> "int"
    | TYPE `Bool -> "bool"

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
      | Ok tok, pos -> tok |> string_of_token |> format_position pos
      | Error e, _ -> string_of_error e
    in
    Printf.fprintf out "%s\n" s

  (** [to_channel lexbuf out] lexes [lexbuf] and writes the results to
      [out] *)
  let to_channel lexbuf out =
    lexbuf |> lex_pos |> List.iter ~f:(print_result out)

  let to_file lexbuf out =
    Out_channel.with_file ~f:(to_channel lexbuf) out

  let file_to_file ~src ~out =
    XiFile.map_same ~f:(fun buf -> to_file buf out) src
end