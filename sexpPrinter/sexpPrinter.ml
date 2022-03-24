open Core

let rec print out = function
  | Sexp.Atom e -> Printf.fprintf out "%s" e
  | Sexp.List es ->
      Printf.fprintf out "(";
      print_list out es;
      Printf.fprintf out ")"

(** [print_list out es] prints the list of s-expressions [es] to out
    channel [out] *)
and print_list out = function
  | [] -> ()
  | [ e ] -> print out e
  | e :: es ->
      print out e;
      Printf.fprintf out " ";
      print_list out es

let pp out =
  let ppf = Format.formatter_of_out_channel out in
  Sexp.pp_hum ppf
