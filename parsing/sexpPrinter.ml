open Core

let print_iter ~n out s =
  for i = 1 to n do
    Printf.fprintf out "%s" s
  done

(** [print_lvl out lvl exp] prints the s-expression *)
let rec print_lvl out lvl sexp =
  match sexp with
  | Sexp.Atom e -> Printf.fprintf out "%s" e
  | Sexp.List es ->
      if lvl > 1 then Printf.fprintf out "\n";
      print_iter ~n:lvl out "  ";
      Printf.fprintf out "(";
      print_list out (succ lvl) es;
      Printf.fprintf out ")"

(** [print_list out es] prints the list of s-expressions [es] to out
    channel [out] *)
and print_list out lvl es =
  print_iter ~n:lvl out "  ";
  match es with
  | [] -> ()
  | [ e ] -> print_lvl out lvl e
  | e :: es ->
      print_lvl out lvl e;
      Printf.fprintf out " ";
      print_list out lvl es

let print out = print_lvl out 1
