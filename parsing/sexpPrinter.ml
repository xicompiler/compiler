open Core

(** [print_lvl out exp] prints the s-expression [exp] to out channel
    [out] *)
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
