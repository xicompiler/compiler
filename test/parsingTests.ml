open OUnit2
open Frontend
open Common

(** [parsing_file_test name ~src ~out ~reference] tests parsing for
    [src], comparing the resulting file with [reference] *)
let parsing_file_test name ~src ~reference =
  let expected = file_contents reference in
  let out = output_file src in
  ignore (Parse.Diagnostic.file_to_file ~src ~out);
  let actual = file_contents out in
  name >:: fun _ ->
  try
    assert_equal
      (Parsexp.Single.parse_string_exn expected)
      (Parsexp.Single.parse_string_exn actual)
  with _ -> assert_equal expected actual

(* Maps each file in [dir] using [parsing_file_test]. *)
let parsing_file_tests =
  map_file_tests ~f:parsing_file_test ".parsedsol"

(** [parsing_file_test_cases] is a list of unit tests for parsing files. *)
let parsing_file_test_cases =
  List.flatten
    [
      parsing_file_tests "./test/parsing/autograder";
      parsing_file_tests "./test/parsing/givenExamples";
      parsing_file_tests "./test/parsing/regression";
      parsing_file_tests "./test/parsing";
    ]

let suite =
  "unit test suite for parsing"
  >::: List.flatten [ parsing_file_test_cases ]
