open OUnit2
open Parsing
open Parsing.Ast
open Parsing.ParserDebug

(** [ast_to_sexpr_test] binds [test_name] to a unit test that asserts that the 
  s-expression representation of [expected] is equal to our converted ast [input]. *)
let ast_to_sexpr_test test_name input expected =
  test_name >:: fun _ -> assert_equal (sexp_of_t input) (Base.String.sexp_of_t expected)

(** [sexpr_test_cases] is a list of unit tests for sexpr conversion. *)
let sexpr_test_cases = [
  (** ast_to_sexpr_test "one use" (Program (Use "fun")) "(()(fun))";
  ast_to_sexpr_test "one function" (Program (Function ({id="nothing"; args=[]; types=[]};
  [Return []]))) "(()(nothing () () (return ())))";
  ast_to_sexpr_test "one assign global" (Program [Global (Assign ([New ("expr", Type.Primitive Int), -1]))]) "(((= (expr int) (- 1))))";
  ast_to_sexpr_test "use io" (Program [Use "io"]) "(((use io)))";
  ast_to_sexpr_test "interface" (Interface [{id="fun"; args=[]; types=[]}]), "(((fun () ())))";
  *)
]


(** [parsing_test_cases] is a list of unit tests for parsing strings. *)
let parsing_test_cases = [
  (**

   *)
]

(** [file_contents in_file] is a string containing the contents of
    [in_file]. *)
let file_contents in_file =
  let ch = open_in in_file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(** [parsing_file_test name ~src ~dst ~reference] constructs an OUnit
    test with name [name] asserting that following
    [parse_to_file ~src ~dst], the contents of [dst] and [reference] are
    equal. *)
let parsing_file_test name ~src ~dst ~reference = 
  let expected = file_contents reference in 
  parse_to_file ~src ~dst;
  let actual = file_contents dst in
  name >:: fun _ -> assert_equal expected actual 

(* Maps each file in [dir] using [parsing_file_test]. *)
let parsing_file_tests dir =
  let make_test file =
    if Filename.extension file = ".xi" then
      let name =
        file |> Filename.remove_extension |> Printf.sprintf "%s/%s" dir
      in
      let src = name ^ ".xi" in
      let dst = name ^ ".output" in
      let reference = name ^ ".parsedsol" in
      Some (parsing_file_test name ~src ~dst ~reference)
    else None
  in
  Sys.readdir dir |> Array.to_list |> List.filter_map make_test

(** [parsing_test_cases] is a list of unit tests for [parse_string]. *)
let parsing_file_test_cases = List.flatten [ 
  parsing_file_tests "./test/parsing/autograder";
  parsing_file_tests "./test/parsing" ]

let parsing_suite =
  List.flatten [ sexpr_test_cases; parsing_test_cases; parsing_file_test_cases ]
