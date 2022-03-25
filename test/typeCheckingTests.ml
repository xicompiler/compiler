open OUnit2
open Frontend
open Common

(** [deps] are the dependencies used for testing *)
let deps : Check.dependencies =
  {
    std_dir = Util.File.stdlib;
    lib_dir = "./test/typecheck/interfaces";
  }

(** [typing_file_test name ~src ~reference] tests typechecking [src],
    comparing the resulting file with [reference] *)
let typing_file_test name ~src ~reference =
  let expected = file_contents reference in
  let out = output_file src in
  ignore (Check.Diagnostic.file_to_file ~src ~out ~deps ());
  let actual = file_contents out in
  name >:: fun _ -> assert_equal expected actual

(** [typing_file_tests path] compiles all test cases in [path] using
    [typing_file_test] *)
let typing_file_tests =
  map_file_tests_xi ~f:typing_file_test ".typedsol"

(** [typing_test_cases] is a list of unit tests for type checking files. *)
let typing_file_test_cases =
  List.flatten
    [
      typing_file_tests "./test/typecheck";
      typing_file_tests "./test/typecheck/autograder";
    ]

let suite =
  "unit test suite for typing"
  >::: List.flatten [ typing_file_test_cases ]
