open Core

type error =
  | LexicalError of Lex.lexical_error
  | SyntaxError of Lex.position

type start = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Ast.t

type parse_result = (Ast.t, error) result

let parse ~start lexbuf =
  try Ok (start Lex.read lexbuf) with
  | Lex.LexicalError err -> Error (LexicalError err)
  | _ ->
      let pos = Lex.get_position lexbuf in
      Error (SyntaxError pos)

let parse_file src =
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
  | LexicalError e -> Lex.string_of_error e
  | SyntaxError pos -> Lex.format_error pos syntax_error_msg

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

let print_syntax_error dst pos =
  Lex.Diagnostic.print_position dst pos syntax_error_msg

module Diagnostic = struct
  (** [print_ast ast dst] prints the S-expression of [ast] into the
      [dst] out channel. *)
  let print_ast ast dst = ast |> Ast.sexp_of_t |> SexpPrinter.print dst

  (** [print_result dst] prints the valid ast S-expression or an error
      message into the [dst] out channel. *)
  let print_result dst = function
    | Ok ast -> print_ast ast dst
    | Error (LexicalError err) -> Lex.Diagnostic.print_error dst err
    | Error (SyntaxError err) -> print_syntax_error dst err

  (** [print_result_file dst] try to print the valid ast S-expression or
      an error message into the file at [dst]. Raises: [Sys_error] if an
      output channel to [dst] cannot be opened. *)
  let print_result_file dst res =
    Out_channel.with_file ~f:(fun oc -> print_result oc res) dst

  let parse_to_file ~src ~dst =
    src |> parse_file |> Option.iter ~f:(print_result_file dst)
end

include Parser