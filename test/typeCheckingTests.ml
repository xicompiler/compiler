open OUnit2
open Frontend
open Util.Test

(** [deps] are the dependencies used for testing *)
let deps : Check.dependencies =
  { std_dir = Util.File.stdlib; lib_dir = "./test/typcheck/interfaces" }

(** [typing_file_test name ~src ~out ~reference] tests typechecking
    [src], comparing the resulting file in [out] with [reference] *)
let typing_file_test name ~src ~out ~reference =
  let expected = file_contents reference in
  ignore (Check.Diagnostic.file_to_file ~src ~out ~deps ());
  let actual = file_contents out in
  name >:: fun _ -> assert_equal expected actual

(** [typing_file_tests path] compiles all test cases in [path] using
    [typing_file_test] *)
let typing_file_tests =
  map_file_tests_no_ixi typing_file_test ".typedsol"

(** [typing_test_cases] is a list of unit tests for type checking files. *)
let typing_file_test_cases =
  List.flatten
    [
      typing_file_tests "./test/typcheck";
      typing_file_tests "./test/typcheck/autograder";
    ]

let typing_suite =
  "unit test suite for typing"
  >::: List.flatten [ typing_file_test_cases ]

let _ = run_test_tt_main typing_suite