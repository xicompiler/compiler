open Core

(** [print_ast ast dst] prints the S-expression of [ast] into the [dst]
    out channel. *)
let print_ast ast dst =
  let ppf = Format.formatter_of_out_channel dst in
  ast |> Ast.sexp_of_t |> Sexp.pp_hum ppf

(** [print_lexical_error err dst] prints the lexical error [err] into
    the [dst] out channel. *)
let print_lexical_error (err : Lexer.lexical_error) dst =
  let cause_msg = Lexer.error_msg err.cause in
  LexerDebug.print_error dst err.position cause_msg

(** [print_syntax_error err dst] prints the syntax error [err] into the
    [dst] out channel. *)
let print_syntax_error (pos : Lexer.position) dst =
  LexerDebug.print_error dst pos "error:Syntax Error"

(** [print_result dst] prints the valid ast S-expression or an error
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
    print_result dst (Frontend.parse lb);
    In_channel.close src;
    Out_channel.close dst
  with
  | e ->
      In_channel.close src;
      Out_channel.close_no_err dst;
      raise e
