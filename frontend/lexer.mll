{
open Parser

type position = {
  line : int;
  column : int;
}

type error_cause =
  | InvalidChar
  | InvalidString
  | InvalidSource

type lexical_error = {
  cause : error_cause;
  position : position;
}

exception LexicalError of lexical_error

let ( >>= ) = Option.bind
let ( >>| ) o f = Option.map f o

(** [parse_escaped_unicode] is [Some c] where [c] the unicode character
    represented by string [s] in the format [\x{n}] for hex number [n],
    or [None] if no such conversion is possible *)
let parse_escaped_unicode s =
  let codepoint = Scanf.sscanf s "\\x{%x}" Fun.id in
  if Uchar.is_valid codepoint then Some (Uchar.of_int codepoint)
  else None

(** [unescaped s] is [Some s] if [s] is [s] unescaped , or [None] if no
    such converstion is possible. *)
let unescaped s =
  try Some (Scanf.unescaped s) with
  | _ -> None

(** [unescaped_char s] is [Some c] if [s] contains the escaped
    unicode character [c], or none otherwise. *)
let unescaped_char s =
  unescaped s >>| fun s -> Uchar.of_char (s.[0])

(** The default length of the string buffer *)
let buf_length = 16

(** [get_or_raise e o] is [x] if [o] is [Some x] and raises [e] otherwise *)
let get_or_raise e = function
  | Some x -> x
  | None -> raise e

(** [col_offset] is the offset between the lexer buffer "column" and the
    true file column *)
let col_offset = 1

let get_position ({ lex_start_p = p; _ } : Lexing.lexbuf) =
  { 
    line = p.pos_lnum; 
    column = col_offset + p.pos_cnum - p.pos_bol 
  }

(** [make_error cause lexbuf] is an [Error] with the specified cause and the
    current position of the lexer buffer. *)
let make_error cause lexbuf =
  LexicalError { cause; position = get_position lexbuf }

(** [parse_ascii_char s] is the first ascii character of the lexeme last
    lexed from [lexbuf], wrapped in a [Uchar.t]. Requires: at least one
    lexeme has been lexed from [lexbuf]. *)
let parse_ascii_char lexbuf =
  Uchar.of_char (Lexing.lexeme_char lexbuf 0)

(** [parse_codepoint s] is the first codepoint of the [s], wrapped in 
    [Some Uchar.t] if present and [None] if absent *)
let parse_codepoint s =
  let d = Uutf.decoder ~encoding:`UTF_8 (`String s) in
  let first = Uutf.decode d in
  let second = Uutf.decode d in
  match (first, second) with
  | `Uchar u, `End -> Some u
  | _ -> None

(** [lex_char_literal read_char lexbuf] is [CHAR c] if
    [read_char lexbuf] is [Some c], and raises [Error] indicating the
    illegal char literal and its location in [lexbuf] *)
let lex_char_literal read_char (lexbuf : Lexing.lexbuf) =
  let start = lexbuf.lex_start_p in
  let err = make_error InvalidChar lexbuf in
  let c = read_char lexbuf in
  lexbuf.lex_start_p <- start;
  CHAR (get_or_raise err c)

(** [lex_string_literal read_string lexbuf] is [STRING s] if
    [read_string lexbuf] is [Some s] and indicating the illegal string
    literal and its location in [lexbuf] *)
let lex_string_literal read_string (lexbuf : Lexing.lexbuf) =
  let start = lexbuf.lex_start_p in
  let err = make_error InvalidString lexbuf in
  let buf = Buffer.create buf_length in
  let s = read_string buf lexbuf in
  lexbuf.lex_start_p <- start;
  STRING (get_or_raise err s)
}

let white = [' ' '\t']+
let newline = '\n'
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let int = digit+
let id = letter (letter | digit | '_' | '\'')*
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let escaped = '\\' (('x' hex hex) | ['n' 'r' 't' 'b' '\\' '\'' '"'])
let non_escape_char = [^ '\\' '\'' '\n']
let non_escape_char_seq = non_escape_char non_escape_char+
let codepoint = hex hex? hex? hex? hex? hex?
let unicode =  "\\x{" codepoint '}'
let any_char = _ | newline

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
  | "length"
    { LENGTH }
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
    { INT i }
  | id as ident
    { ID ident }
  | "_"
    { WILDCARD }
  | "'"
    { lex_char_literal read_char lexbuf }
  | '"'
    { lex_string_literal read_string lexbuf }
  | eof
    { EOF }
  | any_char
    { raise (make_error InvalidSource lexbuf) }

and read_char =
  parse
  | (unicode as u) "'"
    { parse_escaped_unicode u }
  | (escaped as esc) "'"
    { unescaped_char esc }
  | non_escape_char "'"
    { Some (parse_ascii_char lexbuf) }
  | (non_escape_char_seq as s) "'"
    { parse_codepoint s }
  | eof | any_char
    { None }

and read_string buf =
  parse
  | (unicode as u)
    { 
      parse_escaped_unicode u >>= fun u ->
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
      unescaped esc >>= fun s ->
      Buffer.add_string buf s;
      read_string buf lexbuf
    }
  | '"'
    { Some (Buffer.contents buf) }
  | eof | any_char
    { None }

and read_comment =
  parse
  | newline
    { 
      Lexing.new_line lexbuf;
      read lexbuf 
    }
  | eof 
    { EOF }
  | any_char
    { read_comment lexbuf }
