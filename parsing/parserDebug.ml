open Core
open Frontend

(** [print_ast ast dst] prints the S-expression of [ast] into the [dst]
    out channel. *)
let print_ast ast dst = ast |> Ast.sexp_of_t |> SexpPrinter.print dst

(** [print_result dst] prints the valid ast S-expression or an error
    message into the [dst] out channel. *)
let print_result dst = function
  | Ok ast -> print_ast ast dst
  | Error (LexicalError err) -> print_lexical_error dst err
  | Error (SyntaxError err) -> print_syntax_error dst err

(** [print_result_file dst] try to print the valid ast S-expression or
    an error message into the file at [dst]. Raises: [Sys_error] if an
    output channel to [dst] cannot be opened. *)
let print_result_file dst res =
  let oc = Out_channel.create dst in
  try
    print_result oc res;
    Out_channel.close oc
  with
  | e ->
      Out_channel.close_no_err oc;
      raise e

let parse_to_file ~src ~dst =
  src |> parse_file |> Option.iter ~f:(print_result_file dst)
