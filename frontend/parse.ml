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
  let ic = In_channel.create src in
  let lb = Lexing.from_channel ic in
  match Caml.Filename.extension src with
  | ".xi" -> Some (parse ~start:Parser.source lb)
  | ".ixi" -> Some (parse ~start:Parser.interface lb)
  | _ -> None

let syntax_error_msg = "error:Syntax Error"

let ext_error_msg = "error:Invalid Extension"

(** [string_of_error e] is the string representing error [e] *)
let string_of_error = function
  | LexicalError e -> Lex.string_of_error e
  | SyntaxError pos -> Lex.format_position pos syntax_error_msg

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

let print_syntax_error out pos =
  Lex.Diagnostic.print_position out pos syntax_error_msg

module Diagnostic = struct
  (** [print_ast ast out] prints the S-expression of [ast] into the
      [out] out channel. *)
  let print_ast ast out = ast |> Ast.sexp_of_t |> SexpPrinter.print out

  (** [print_result out] prints the valid ast S-expression or an error
      message into the [out] out channel. *)
  let print_result out = function
    | Ok ast -> print_ast ast out
    | Error (LexicalError err) -> Lex.Diagnostic.print_error out err
    | Error (SyntaxError err) -> print_syntax_error out err

  (** [print_result_file out] try to print the valid ast S-expression or
      an error message into the file at [out]. Raises: [Sys_error] if an
      output channel to [out] cannot be opened. *)
  let print_result_file out res =
    Out_channel.with_file ~f:(fun oc -> print_result oc res) out

  let parse_to_file ~src ~out =
    src |> parse_file |> Option.iter ~f:(print_result_file out)
end

include Parser