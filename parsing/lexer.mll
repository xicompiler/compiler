{
open Parser

type position = {
  line : int;
  column : int;
}

type lexical_error =
  | InvalidChar
  | InvalidString
  | InvalidSource

exception
  Error of {
    cause : lexical_error;
    position : position;
  }

let ( >>= ) = Option.bind
let ( >>| ) o f = Option.map f o

(** [parse_unicode] is [Some c] where [c] the unicode character
    represented by string [s] in the format [\x{n}] for hex number [n],
    or [None] if no such conversion is possible *)
let parse_unicode s =
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

(** [get_position lexbuf] is the current position of [lexbuf] *)
let get_position ({ lex_start_p = p; _ } : Lexing.lexbuf) =
  { line = p.pos_lnum; column = p.pos_cnum - p.pos_bol }

(** [error cause lexbuf] is an [Error] with the specified cause and the
    current position of the lexer buffer. *)
let error cause lexbuf =
  Error { cause; position = get_position lexbuf }

(** [parse_ascii_char lexbuf] is the first character of the lexeme most
    recently consumed by [lexbuf], wrapped in [Uchar.t]. Requires:
    [lexbuf] has consumed a valid lexeme. *)
let parse_ascii_char lexbuf =
  Uchar.of_char (Lexing.lexeme_char lexbuf 0)

(** [lex_char_literal read_char lexbuf] is [CHAR c] if
    [read_char lexbuf] is [Some c], and raises [Error] indicating the
    illegal char literal and its location in [lexbuf] *)
let lex_char_literal read_char lexbuf =
  let err = error InvalidChar lexbuf in
  CHAR (lexbuf |> read_char |> get_or_raise err)

(** [lex_string_literal read_string lexbuf] is [STRING s] if
    [read_string lexbuf] is [Some s] and indicating the illegal string
    literal and its location in [lexbuf] *)
let lex_string_literal read_string lexbuf =
  let err = error InvalidString lexbuf in
  let buf = Buffer.create buf_length in
  STRING (lexbuf |> read_string buf |> get_or_raise err)
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
    { lex_char_literal read_char lexbuf }
  | '"'
    { lex_string_literal read_string lexbuf }
  | eof
    { EOF }
  | any_char
    { raise (error InvalidSource lexbuf) }

and read_char =
  parse
  | newline
    { 
      Lexing.new_line lexbuf;
      Error Newline
    }
  | (unicode as u) "'"
    { parse_unicode u }
  | [^ '\\' '\'']
    { Some (parse_ascii_char lexbuf) }
  | (escaped as esc) "'"
    { unescaped_char esc }
  | eof | any_char
    { None }

and read_string buf =
  parse
  | newline
    { 
      Lexing.new_line lexbuf;
      Error Newline
    }
  | (unicode as u)
    { 
      parse_unicode u >>= fun u ->
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

{

(** [lex buf] is the list of all tokens lexed from buffer [buf], excluding [EOF] *)
let rec lex_tok buf =
  match read buf with
  | EOF -> []
  | tok -> tok :: lex_tok buf

let lex_tok_string s = s |> Lexing.from_string |> lex_tok

let rec lex_pos_tok buf =
  let token = read buf in
  let pos = get_position buf in
  match token with
  | EOF -> []
  | tok -> (pos, tok) :: lex_pos_tok buf
  | exception e -> let {line;column} = pos in
      Printf.printf "%d:%d error\n" line column;
      raise e

let lex_pos_tok_string s = s |> Lexing.from_string |> lex_pos_tok

let lex in_file out_file =
  let print_token oc (pos, tok) = 
    let {line;column} : token_position = pos in
    Printf.fprintf oc "%d:%d\n" line column;
    flush stdout; in

  let file_contents =
    let ch = open_in in_file in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s in

  let oc = open_out out_file in begin
    try
      file_contents |> lex_pos_tok_string |> List.iter (print_token oc);
      close_out oc;
    with e ->
      close_out_noerr oc;
      raise e
  end
}
