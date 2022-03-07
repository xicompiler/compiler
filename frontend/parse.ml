open Core
open Position

type syntax_error = string Position.error

module Error = struct
  type error =
    | LexicalError of Lex.error
    | SyntaxError of syntax_error
end

type 'a start = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a
type nonrec 'a result = ('a, error) result

let parse ~start lexbuf =
  try Ok (start Lex.read lexbuf) with
  | Lex.Error err -> Error (LexicalError err)
  | Parser.Error ->
      let pos = Position.get_position_lb lexbuf in
      let cause = Lexing.lexeme lexbuf in
      Error (SyntaxError (Error.make ~pos cause))

let parse_prog = parse ~start:Parser.prog
let parse_source = parse ~start:Parser.source
let parse_intf = parse ~start:Parser.intf

(** [error_description e] is the error description corresponding to [e] *)
let error_description cause =
  if String.is_empty cause then "Unable to parse program"
  else Printf.sprintf "Unexpected token %s" cause

(** [string_of_error_cause e] is the error message corresponding to [e] *)
let string_of_error_cause cause =
  Printf.sprintf "error:%s" (error_description cause)

let string_of_error filename = function
  | LexicalError e -> Lex.Error.to_string filename e
  | SyntaxError e ->
      let pos = Error.position e in
      e |> Error.cause |> error_description
      |> format_position_error pos
      |> Printf.sprintf "Syntax error beginning at %s:%s" filename

(** [ast_of_source read lexbuf] wraps [Parser.source read lexbuf] up as
    [Ast.t]*)
let ast_of_source read lexbuf = Ast.Source (Parser.source read lexbuf)

(** [ast_of_intf read lexbuf] wraps [Parser.intf read lexbuf] up as
    [Ast.t]*)
let ast_of_intf read lexbuf = Ast.Intf (Parser.intf read lexbuf)

let bind ~f =
  XiFile.bind ~source:(f ast_of_source) ~intf:(f ast_of_intf)

let map ~f = XiFile.map ~source:(f ast_of_source) ~intf:(f ast_of_intf)

module Diagnostic = struct
  (** [print_ast out ast] prints the S-expression of [ast] into the
      [out] out channel. *)
  let print_ast out ast = ast |> Ast.sexp_of_t |> SexpPrinter.print out

  (** [string_of_error e] is the string representing error [e] *)
  let string_of_error = function
    | LexicalError e -> Lex.Diagnostic.Error.to_string e
    | SyntaxError e ->
        let pos = Error.position e in
        e |> Error.cause |> string_of_error_cause
        |> Position.Error.format pos

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