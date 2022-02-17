open Core
open Frontend

(** [print_ast ast dst] prints the S-expression of [ast] into the [dst]
    out channel. *)
let print_ast ast dst = ast |> Ast.sexp_of_t |> SexpPrinter.print dst

(** [print_result dst] prints the valid ast S-expression or an error
    message into the [dst] out channel. *)
let print_result dst = function
  | Ok ast -> print_ast ast dst
  | Error (LexicalError err) -> print_lexical_error err dst
  | Error (SyntaxError err) -> print_syntax_error err dst

let parse_to_file ~src ~dst =
  let ic = In_channel.create src in
  let oc = Out_channel.create dst in
  try
    print_result oc (parse_file src);
    In_channel.close ic;
    Out_channel.close oc
  with
  | e ->
      In_channel.close ic;
      Out_channel.close_no_err oc;
      raise e
