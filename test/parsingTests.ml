open OUnit2
open Frontend

(** [file_contents in_file] is a string containing the contents of
    [in_file]. *)
let file_contents in_file =
  let ch = open_in in_file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

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
  with
  | _ -> assert_equal expected actual

(* Maps each file in [dir] using [parsing_file_test]. *)
let parsing_file_tests dir =
  let make_test file =
    let ext = Filename.extension file in
    if ext = ".xi" || ext = ".ixi" then
      let name =
        file |> Filename.remove_extension |> Printf.sprintf "%s/%s" dir
      in
      let src = name ^ ext in
      let out = name ^ ".output" in
      let reference = name ^ ".parsedsol" in
      Some (parsing_file_test name ~src ~out ~reference)
    else None
  in
  Sys.readdir dir |> Array.to_list |> List.filter_map make_test

(** [parsing_test_cases] is a list of unit tests for [parse_string]. *)
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
