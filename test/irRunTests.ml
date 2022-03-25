open OUnit2
open Frontend
open Common

(** [ir_file_test name ~src ~out ~reference] tests ir generation [src],
    comparing the resulting file in [out] with [reference] *)
let ir_file_test name ~src ~out ~reference =
  let expected = file_contents reference in
  let src_no_ext = Filename.remove_extension src in
  let command =
    Printf.sprintf "./xic --irrun %s > %s.output" src src_no_ext
  in
  ignore (Sys.command command);
  let actual = file_contents out in
  name >:: fun _ -> assert_equal expected actual

(** [ir_file_tests path] compiles all test cases in [path] using
    [ir_file_test] *)
let ir_file_tests = map_file_tests_no_ixi ir_file_test ".irsol.nml"

(** [ir_file_test_cases] is a list of unit tests for ir generation
    files. *)
let ir_file_test_cases =
  List.flatten
    [
      ir_file_tests "./test/irgen";
      ir_file_tests "./test/irgen/autograder";
    ]

let suite =
  "unit test suite for irgen and irrun"
  >::: List.flatten [ ir_file_test_cases ]