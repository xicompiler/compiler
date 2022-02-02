{
open Parser
exception InvalidChar
exception InvalidString

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

(** [get_or_raise e o] is [x] if [o] is [Some x]. Raises: [e] if [o] is
    [None] *)
let get_or_raise e = function
  | Some x -> x
  | None -> raise e

(** The default length of the string buffer *)
let buf_length = 16
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
  | '\''
    { read_char lexbuf }
  | "\""
    { read_string (Buffer.create buf_length) lexbuf }
  | eof
    { EOF }

and read_char =
  parse
  | (unicode as u) '\''
    { CHAR (u |> parse_unicode |> get_or_raise InvalidChar) }
  | [^ '\\' '\'']
    { CHAR (0 |> Lexing.lexeme_char lexbuf |> Uchar.of_char) }
  | (escaped as esc) '\''
    { CHAR (esc |> unescaped_char |> get_or_raise InvalidChar) }
  | _ | eof
    { raise InvalidChar }

and read_string buf =
  parse
  | (unicode as u)
    { 
      u
      |> parse_unicode 
      |> get_or_raise InvalidString 
      |> Buffer.add_utf_8_uchar buf;
      read_string buf lexbuf
    }
  | [^ '\\' '\"']
    { 
      0
      |> Lexing.lexeme_char lexbuf
      |> Buffer.add_char buf;
      read_string buf lexbuf
    }
  | (escaped as esc)
    {
      esc
      |> unescaped_char
      |> get_or_raise InvalidString
      |> Buffer.add_utf_8_uchar buf;
      read_string buf lexbuf
    }
  | '"'
    { STRING (Buffer.contents buf) }
  | _ | eof
    { raise InvalidChar }

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
}
