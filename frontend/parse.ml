open Core
open Position

type syntax_error = string Position.error

type error =
  | LexicalError of Lex.error
  | SyntaxError of syntax_error

type start = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> Ast.t
type nonrec result = (Ast.t, error) result

let parse ~start lexbuf =
  try Ok (start Lex.read lexbuf) with
  | Lex.Error err -> Error (LexicalError err)
  | Parser.Error ->
      let pos = Position.get_position_lb lexbuf in
      let cause = Lexing.lexeme lexbuf in
      Error (SyntaxError (Error.make ~pos cause))

let error_description cause =
  if String.is_empty cause then "Unable to parse program"
  else Printf.sprintf "Unexpected token %s" cause

let string_of_error_cause cause =
  Printf.sprintf "error:%s" (error_description cause)

(** [string_of_error e] is the string representing error [e] *)
let string_of_error = function
  | LexicalError e -> Lex.string_of_error e
  | SyntaxError e ->
      let pos = Error.position e in
      e |> Error.cause |> string_of_error_cause
      |> Lex.format_position pos

let cli_string_of_error filename = function
  | LexicalError e -> Lex.cli_string_of_error filename e
  | SyntaxError e ->
      let { line; column } = Error.position e in
      Printf.sprintf "Syntax error beginning at %s:%d:%d: Syntax Error"
        filename line column

let bind ~f =
  XiFile.bind ~source:(f Parser.source) ~interface:(f Parser.interface)

let map ~f =
  XiFile.map ~source:(f Parser.source) ~interface:(f Parser.interface)

module Diagnostic = struct
  (** [print_ast out ast] prints the S-expression of [ast] into the
      [out] out channel. *)
  let print_ast out ast = ast |> Ast.sexp_of_t |> SexpPrinter.print out

  (** [print_result out] prints the valid ast S-expression or an error
      message into the [out] out channel. *)
  let print_result out = function
    | Ok ast -> print_ast out ast
    | Error e -> Printf.fprintf out "%s\n" (string_of_error e)

  (** [to_channel ~start lexbuf out] parses lexer buffer [lexbuf] from
      start symbol [start] and writes the diagnostic output to [out] *)
  let to_channel ~start lexbuf out =
    lexbuf |> parse ~start |> print_result out

  let to_file ~start lexbuf out =
    Out_channel.with_file ~f:(to_channel ~start lexbuf) out

  let file_to_file ~src ~out =
    bind ~f:(fun start lexbuf -> Ok (to_file ~start lexbuf out)) src
end

include Parser