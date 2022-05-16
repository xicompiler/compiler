open Core
open Result.Monad_infix

module Error = struct
  type syntax_error = string Position.error

  type t =
    [ `LexicalError of Lex.error
    | `SyntaxError of syntax_error
    ]

  (** [desc e] is the error description corresponding to [e] *)
  let desc cause =
    if String.is_empty cause then "Unable to parse program"
    else Printf.sprintf "Unexpected token %s" cause

  (** [string_of_cause e] is the error message corresponding to [e] *)
  let string_of_cause cause = cause |> desc |> Printf.sprintf "error:%s"

  (** [fmt] is the format for a lexical error message *)
  let fmt = format_of_string "Syntax error beginning at %s:%s"

  let to_string filename = function
    | `LexicalError e -> Lex.Error.to_string filename e
    | `SyntaxError e ->
        Position.Error.format_last ~f:desc ~fmt ~msg:filename e
end

type error = Error.t

exception Exn of error

type 'a start = (Lexing.lexbuf -> Parser.token) -> Lexing.lexbuf -> 'a
type nonrec 'a result = ('a, error) result

let parse ~is_rho ~start lexbuf =
  let read = if is_rho then Lex.read_rho else Lex.read_xi in
  try Ok (start read lexbuf) with
  | Lex.Error err -> Error (`LexicalError err)
  | Parser.Error ->
      let pos = Position.of_lexbuf lexbuf in
      let cause = Lexing.lexeme lexbuf in
      Error (`SyntaxError (Position.Error.create ~pos cause))
  | Exception.InvalidIntLiteral err -> Error (`SyntaxError err)

let parse_prog ~is_rho =
  let start = if is_rho then Parser.rho_prog else Parser.xi_prog in
  parse ~is_rho ~start

let parse_source ~is_rho =
  let start = if is_rho then Parser.rho_source else Parser.xi_source in
  parse ~is_rho ~start

let parse_intf ~is_rho =
  let start = if is_rho then Parser.rho_intf else Parser.xi_intf in
  parse ~is_rho ~start

(** [ast_of_source read lexbuf] wraps [Parser.source read lexbuf] up as
    [Ast.Undecorated.t]*)
let ast_of_source ~is_rho read lexbuf =
  let source = if is_rho then Parser.rho_source else Parser.xi_source in
  Ast.Source (source read lexbuf)

(** [ast_of_intf read lexbuf] wraps [Parser.intf read lexbuf] up as
    [Ast.Undecorated.t]*)
let ast_of_intf ~is_rho read lexbuf =
  let intf = if is_rho then Parser.rho_intf else Parser.xi_intf in
  Ast.Intf (intf read lexbuf)

let map ~is_rho ~start ~f buf = parse ~is_rho ~start buf >>| f

module Diagnostic = struct
  (** [print_ast out ast] prints the S-expression of [ast] into the
      [out] out channel. *)
  let print_ast out ast = ast |> Ast.sexp_of_t |> SexpPrinter.print out

  module Error = struct
    let to_string = function
      | `LexicalError e -> Lex.Diagnostic.Error.to_string e
      | `SyntaxError e ->
          Position.Error.format ~f:Error.string_of_cause e
  end

  (** [print_error out e] prints the error [e] to out channel [out] *)
  let print_error out e = Printf.fprintf out "%s\n" (Error.to_string e)

  (** [print_result out] prints the valid ast S-expression or an error
      message into the [out] out channel. *)
  let print_result ~out = function
    | Ok ast -> print_ast out ast
    | Error e -> print_error out e

  (** [to_channel ~start lexbuf out] parses lexer buffer [lexbuf] from
      start symbol [start] and writes the diagnostic output to [out] *)
  let to_channel ~is_rho ~start lexbuf out =
    lexbuf |> parse ~is_rho ~start |> print_result ~out

  (** [to_file ~start lexbuf out] parses lexer buffer [lexbuf] from
      start symbol [start] and writes the diagnostic output to file at
      path [out] *)
  let to_file ~is_rho ~start lexbuf out =
    Out_channel.with_file ~f:(to_channel ~is_rho ~start lexbuf) out

  let file_to_file ~src ~out =
    let is_rho = Util.File.is_rh src in
    let source lexbuf =
      to_file ~is_rho ~start:(ast_of_source ~is_rho) lexbuf out
    in
    let intf lexbuf =
      to_file ~is_rho ~start:(ast_of_intf ~is_rho) lexbuf out
    in
    File.Xi.map_same ~source ~intf src
end

module File = struct
  type 'a source = Ast.Undecorated.source -> 'a
  type 'a intf = Ast.Undecorated.intf -> 'a

  (** Same as [map] but both [source] and [intf] return the same type *)
  let map_same ~is_rho ~source ~intf file : 'a result File.Xi.result =
    let source buf = parse_source ~is_rho buf >>| source in
    let intf buf = parse_intf ~is_rho buf >>| intf in
    File.Xi.map_same ~source ~intf file

  let map_ast ~f file =
    let source = Fn.compose f Ast.source in
    let intf = Fn.compose f Ast.intf in
    let is_rho = Util.File.is_rh file in
    map_same ~is_rho ~source ~intf file

  let parse_intf file =
    let is_rho = Util.File.is_rh file in
    File.map ~f:(parse_intf ~is_rho) file
end

include Parser
