open Core

(** [print_ast ast dst] prints the s-expression of [ast] into the [dst]
    out channel. *)
let print_ast ast dst =
  let ppf = Format.formatter_of_out_channel dst in
  ast |> Ast.sexp_of_t |> Sexp.pp_hum ppf;
  Ok ()

let format_error msg ({ line; column } : Lexer.position) =
  Printf.sprintf "%d:%d %s\n" line column msg

(** [print_lexical_error err dst] prints the lexical error [err] into
    the [dst] out channel. *)
let print_lexical_error (err : Lexer.lexical_error) dst =
  let cause_msg = Lexer.error_msg err.cause in
  let err_msg = format_error cause_msg err.position in
  Printf.fprintf dst "%s\n" err_msg;
  Error err_msg

(** [print_syntax_error err dst] prints the syntax error [err] into the
    [dst] out channel. *)
let print_syntax_error (err : Lexer.position) dst =
  let err_msg = format_error "Syntax Error" err in
  Printf.fprintf dst "%s\n" err_msg;
  Error err_msg

(** [print_result dst] prints the valid ast s-expression or an error
    message into the [dst] out channel. *)
let print_result dst = function
  | Ok ast -> print_ast ast dst
  | Error (Frontend.LexicalError err) -> print_lexical_error err dst
  | Error (Frontend.SyntaxError err) -> print_syntax_error err dst

let parse_to_file ~src ~dst =
  let src = In_channel.create src in
  let dst = Out_channel.create dst in
  try
    let lb = Lexing.from_channel src in
    let res = print_result dst (Frontend.parse lb) in
    In_channel.close src;
    Out_channel.close dst;
    res
  with
  | e ->
      In_channel.close src;
      Out_channel.close_no_err dst;
      raise e
