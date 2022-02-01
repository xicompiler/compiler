{
  open Parser
  exception InvalidChar

  (** [escaped_char c] is the character represented by string [c] *)
  let escaped_char = function
    | "\\" -> '\\'
    | "\\n" -> '\n'
    | "\\'" -> '\''
    | _ -> failwith "Precondition violation"

  let splice_end s = String.sub s 0 ((String.length s) - 1)

  let char_of_string s = s.[0]

  let parse_unicode s =
    let start = 3 in
    String.length s - 4
    |> String.sub s start
    |> ( ^ ) "0x"
    |> int_of_string_opt
    |> Option.map Char.chr
}

let white = [' ' '\t']+
let newline = '\n'
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'z']
let int = '-'? digit+
let id = letter (letter | digit | '_' | '\'')*
let hex = ['0'-'9' 'a'-'f' 'A'-'F']
let codepoint = hex hex? hex? hex? hex? hex?
let escaped = '\\' ('\\' | 'n' | '\'')
let unicode = '\\' 'x' '{' codepoint '}'

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
    { read_char lexbuf }
  | "\""
    { read_string lexbuf }
  | eof
    { EOF }

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

and read_char =
  parse
  | escaped "'" as c
    { 
      CHAR (c |> splice_end |> escaped_char)
    }
  | unicode "'" as u
    {
      match u |> splice_end |> parse_unicode with
      | None -> raise InvalidChar
      | Some c -> CHAR c
    }
  | _ "'" as c
    { CHAR (c |> splice_end |> char_of_string) }
  | _* "'"
    { raise InvalidChar }

and read_string =
  parse
  | "\\\""
    { STRING "escaped quote" }
  | "\""
    { STRING "end" }
  | _
    { STRING "char" }
