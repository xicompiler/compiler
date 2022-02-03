{
open Parser

exception LexicalError

type token_position = {
  line : int;
  column : int;
}

exception InvalidChar of token_position
exception InvalidString of token_position

(** A [literal_error] describes a lexical error due to a error in string or character literal *)
type literal_error =
  | InvalidEscape of string
  | InvalidUnicode of string
  | Newline
  | EOF
  | Other

(** [parse_unicode] is [Some c] where [c] the unicode character
    represented by string [s] in the format [\x{n}] for hex number [n],
    or [None] if no such conversion is possible *)
let parse_unicode s =
  let codepoint = Scanf.sscanf s "\\x{%x}" Fun.id in
  if Uchar.is_valid codepoint then Some (Uchar.of_int codepoint)
  else None

(** [unescaped_char s] is [Some c] if [c] is the unicode charcter
    represented by escaped string [s], or [None] if no such converstion
    is possible. *)
let unescaped_char s =
  try
    let unesc = Scanf.unescaped s in
    0 |> String.get unesc |> Uchar.of_char |> Option.some
  with
  | _ -> None

(** The default length of the string buffer *)
let buf_length = 16

(** [ok_or_raise e r] is [x] if [r] is [Ok x] and raises [e] otherwise *)
let ok_or_raise e = function
  | Ok x -> x
  | Error _ -> raise e

let get_position (lb : Lexing.lexbuf) =
  let p = lb.lex_start_p in {
    line = p.pos_lnum;
    column = p.pos_cnum - p.pos_bol;
  }

let (>>=) = Result.bind
}

let white = [' ' '\t']+
let newline = '\n'
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let int = '-'? digit+
let id = letter (letter | digit | '_' | '\'')*
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let escaped = '\\' (('x' hex hex) | _)
let codepoint = hex hex? hex? hex? hex? hex?
let unicode =  "\\x{" codepoint '}'

rule read =
  parse
  | newline
    { 
      Lexing.new_line lexbuf;
      read lexbuf
    }
  | white
    { read lexbuf }
  | "//"
    { read_comment lexbuf }
  | "use"
    { USE }
  | "if" 
    { IF }
  | "else" 
    { ELSE }
  | "while"
    { WHILE }
  | "return"
    { RETURN }
  | "("
    { LPAREN }
  | ")"
    { RPAREN }
  | "["
    { LBRACKET }
  | "]"
    { RBRACKET }
  | "{"
    { LBRACE }
  | "}"
    { RBRACE }
  | "="
    { GETS }
  | "*"
    { MULT }
  | "*>>"
    { HIGHMULT }
  | "/"
    { DIV }
  | "%"
    { MOD }
  | "+"
    { PLUS }
  | "-"
    { MINUS }
  | "<"
    { LT }
  | "<="
    { LEQ }
  | ">="
    { GEQ }
  | ">"
    { GT }
  | "=="
    { EQ }
  | "!="
    { NEQ }
  | "!"
    { NOT }
  | "&"
    { AND }
  | "|"
    { OR }
  | ":"
    { COLON }
  | ";"
    { SEMICOLON }
  | ","
    { COMMA }
  | "int"
    { TYPE Type.Int }
  | "bool"
    { TYPE Type.Bool }
  | "true"
    { BOOL true }
  | "false"
    { BOOL false }
  | int as i
    { INT (int_of_string i) }
  | id as ident
    { ID ident }
  | "'"
    {
      let pos = get_position lexbuf in
      CHAR (lexbuf |> read_char |> ok_or_raise (InvalidChar pos))
    }
  | '"'
    {
      let pos = get_position lexbuf in
      let buf = Buffer.create buf_length in
      STRING (lexbuf |> read_string buf |> ok_or_raise (InvalidString pos))
    }
  | eof
    { EOF }
  | _
    {
      raise LexicalError
    }

and read_char =
  parse
  | newline
    { 
      Lexing.new_line lexbuf;
      Error Newline
    }
  | (unicode as u) "'"
    { u |> parse_unicode |> Option.to_result ~none:(InvalidUnicode u) }
  | [^ '\\' '\'' '\n']
    { 0 |> Lexing.lexeme_char lexbuf |> Uchar.of_char |> Result.ok }
  | (escaped as esc) "'"
    { esc |> unescaped_char |> Option.to_result ~none:(InvalidEscape esc) }
  | eof
    { Error EOF }
  | _
    { Error Other }

and read_string buf =
  parse
  | newline
    { 
      Lexing.new_line lexbuf;
      Error Newline
    }
  | (unicode as u)
    { 
      u
      |> parse_unicode 
      |> Option.to_result ~none:(InvalidUnicode u) >>= fun u -> 
      Buffer.add_utf_8_uchar buf u;
      read_string buf lexbuf
    }
  | [^ '\\' '"' '\n']+ as s
    { 
      Buffer.add_string buf s;
      read_string buf lexbuf 
    }
  | (escaped as esc)
    {
      esc
      |> unescaped_char
      |> Option.to_result ~none:(InvalidEscape esc) >>= fun u ->
      Buffer.add_utf_8_uchar buf u;
      read_string buf lexbuf
    }
  | '"'
    { Ok (Buffer.contents buf) }
  | eof
    { Error EOF }
  | _
    { Error Other }

and read_comment =
  parse
  | newline
    { 
      Lexing.new_line lexbuf;
      read lexbuf 
    }
  | eof 
    { EOF }
  | _
    { read_comment lexbuf }

{
(** [lex buf] is the list of all tokens lexed from buffer [buf], excluding [EOF] *)
let rec lex buf =
  match read buf with
  | EOF -> []
  | tok -> tok :: lex buf

let lex_string s = s |> Lexing.from_string |> lex

let rec lex_pos buf =
  let pos = get_position buf in
  match read buf with
  | EOF -> []
  | tok -> (pos, tok) :: lex_pos buf
  | exception e -> let {line;column} = pos in
      Printf.printf "%d:%d error\n" line column;
      raise e

let lex_pos_string s = s |> Lexing.from_string |> lex_pos
}
