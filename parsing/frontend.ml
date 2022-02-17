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
  print_endline src;
  let ext = Caml.Filename.extension src in
  let src = In_channel.create src in
  let lb = Lexing.from_channel src in
  match ext with
  | ".xi" -> Some (parse ~start:Parser.source lb)
  | ".ixi" -> Some (parse ~start:Parser.interface lb)
  | _ -> None

let syntax_error_msg = "error:Syntax Error"

let ext_error_msg = "error:Invalid Extension"

(** [string_of_error e] is the string representing error [e] *)
let string_of_error = function
  | LexicalError e -> Lexer.string_of_error e
  | SyntaxError pos -> Lexer.format_error pos syntax_error_msg

(** [fold_error msg acc] is [acc] with error message [msg] folded in*)
let fold_error msg = function
  | Ok () -> Error [ msg ]
  | Error es -> Error (msg :: es)

(** [fold_file acc file] is [acc] with the result of parsing [file]
    folded in *)
let fold_file acc file =
  match parse_file file with
  | Some (Ok _) -> acc
  | Some (Error e) -> fold_error (string_of_error e) acc
  | None -> fold_error ext_error_msg acc

let parse_files files =
  let init = Ok () in
  files
  |> List.fold_left ~f:fold_file ~init
  |> Result.map_error ~f:List.rev

let print_lexical_error dst (err : Lexer.lexical_error) =
  err.cause |> Lexer.string_of_error_cause
  |> LexerDebug.print_error dst err.position

let print_syntax_error dst pos =
  LexerDebug.print_error dst pos syntax_error_msg
