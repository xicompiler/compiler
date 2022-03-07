open OUnit2
open Frontend
open Util

(** [parsing_file_test name ~src ~out ~reference] constructs an OUnit
    test with name [name] asserting that following
    [parse_to_file ~src ~out], the contents of [out] and [reference] are
    equal. *)
let parsing_file_test name ~src ~out ~reference =
  let expected = file_contents reference in
  Result.get_ok (Parse.Diagnostic.file_to_file ~src ~out);
  let actual = file_contents out in
  name >:: fun _ ->
  try
    assert_equal
      (Parsexp.Single.parse_string_exn expected)
      (Parsexp.Single.parse_string_exn actual)
  with _ -> assert_equal expected actual

(* Maps each file in [dir] using [parsing_file_test]. *)
let parsing_file_tests = map_file_tests parsing_file_test ".parsedsol"

(** [parsing_file_test_cases] is a list of unit tests for parsing files. *)
let parsing_file_test_cases =
  List.flatten
    [
      parsing_file_tests "./test/parsing/autograder";
      parsing_file_tests "./test/parsing/givenExamples";
      parsing_file_tests "./test/parsing/regression";
      parsing_file_tests "./test/parsing";
    ]

let parsing_suite =
  "unit test suite for parsing"
  >::: List.flatten [ parsing_file_test_cases ]

let _ = run_test_tt_main parsing_suite
