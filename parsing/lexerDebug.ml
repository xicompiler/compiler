open Lexer
open Parser

type result = (Parser.token, lexical_error) Result.t

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
  | TYPE Type.Int -> "int"
  | TYPE Type.Bool -> "bool"

let read_result lexbuf =
  try Ok (read lexbuf) with
  | LexicalError e -> Result.Error e

(** [lex_pos_rev lexbuf] is a reversed list of [(result, position)]
    pairs of all tokens lexed from lexbuf *)
let lex_pos_rev lexbuf =
  let rec help acc =
    let res = read_result lexbuf in
    let pos = get_position lexbuf in
    match res with
    | Ok EOF -> acc
    | Ok _ -> help ((res, pos) :: acc)
    | Result.Error _ -> (res, pos) :: acc
  in
  help []

let lex_pos lexbuf =
  let flatten (res, pos) =
    let res' = Result.map_error (fun e -> e.cause) res in
    (res', pos)
  in
  lexbuf |> lex_pos_rev |> List.rev_map flatten

let lex lexbuf = lexbuf |> lex_pos_rev |> List.rev_map fst

let print_error out ({ line; column } : Lexer.position) =
  Printf.fprintf out "%d:%d %s\n" line column

let lex_string s = s |> Lexing.from_string |> lex

(** [string_of_result r] is the string representation of [r] *)
let string_of_result = function
  | Ok tok -> string_of_token tok
  | Result.Error e -> Lexer.string_of_error_cause e

let lex_to_channel ~src ~dst =
  let print_result (res, pos) =
    res |> string_of_result |> print_error dst pos
  in
  src |> Lexing.from_channel |> lex_pos |> List.iter print_result

let lex_to_file ~src ~dst =
  let src = open_in src in
  let dst = open_out dst in
  try
    lex_to_channel ~src ~dst;
    close_in src;
    close_out dst
  with
  | e ->
      close_in_noerr src;
      close_out_noerr dst;
      raise e
