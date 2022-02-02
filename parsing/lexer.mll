{
open Parser
exception InvalidChar

(** [parse_unicode s e] is the unicode character represented by string
    [s]. Raises: [e] if [s] is in the format [\x{n}] where [n]
    is a valid hexidecimal number. *)
let parse_unicode s e =
  let codepoint = Scanf.sscanf s "\\x{%x}" Fun.id in
  if Uchar.is_valid codepoint then codepoint else raise e

(** [unescaped_byte s e] is the byte represented by the escaped string
    [s]. Raises: [e] if no such conversion is possible *)
let unescaped_byte s e =
  try
    let unesc = Scanf.unescaped s in
    int_of_char (String.get unesc 0)
  with
  | _ -> raise e

}

let white = [' ' '\t']+
let newline = '\n'
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'z']
let int = '-'? digit+
let id = letter (letter | digit | '_' | '\'')*
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let codepoint = hex hex? hex? hex? hex? hex?
let escaped = '\\' (('x' hex hex) | _)
let unicode =  "\\x{" codepoint '}'
let ascii_char_literal = [^ '\\' '\''] | escaped
let ascii_string_literal = [^ '\\' '"'] | escaped

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
    { failwith "lexing string literals: unimplemented" }
  | eof
    { EOF }

and read_char =
  parse
  | (unicode as u) '\''
    { CHAR (parse_unicode u InvalidChar) }
  | (ascii_char_literal as c) '\''
    { CHAR (unescaped_byte c InvalidChar) }
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
