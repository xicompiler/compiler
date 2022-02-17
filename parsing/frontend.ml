open Parser
open Core

type error =
  | LexicalError of Lexer.lexical_error
  | SyntaxError of Lexer.position

type start = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Ast.t

type parse_result = (Ast.t, error) result

let parse ~start lexbuf =
  try Ok (start Lexer.read lexbuf) with
  | Lexer.LexicalError err -> Error (LexicalError err)
  | _ ->
      let pos = Lexer.get_position lexbuf in
      Error (SyntaxError pos)

let parse_file src =
  let _, ext = Filename.split_extension src in
  let src = In_channel.create src in
  let lb = Lexing.from_channel src in
  match ext with
  | Some ".xi" -> parse ~start:Parser.source lb
  | Some ".ixi" -> parse ~start:Parser.interface lb
  | _ -> parse ~start:Parser.program lb

(** [acc_error e acc] is [\[e\]] if [acc] is [Ok ()] and is
    [e :: errors] if [acc] is [Error errors]. *)
let acc_error e = function
  | Ok () -> [ e ]
  | Error errors -> e :: errors

let parse_files files =
  let f acc src =
    match parse_file src with
    | Ok _ -> acc
    | Error e -> Error (acc_error e acc)
  in
  List.fold_left files ~init:(Ok ()) ~f

let print_lexical_error (err : Lexer.lexical_error) dst =
  let cause_msg = Lexer.error_msg err.cause in
  LexerDebug.print_error dst err.position cause_msg

let print_syntax_error (pos : Lexer.position) dst =
  LexerDebug.print_error dst pos "error:Syntax Error"

let print_errors errors =
  let open Out_channel in
  let f = function
    | LexicalError e -> print_lexical_error e stdout
    | SyntaxError e -> print_syntax_error e stdout
  in
  List.iter errors ~f
