open OUnit2
open Common

(** [deps] are the dependencies used for testing *)
let deps : Frontend.Check.dependencies =
  {
    std_dir = Util.File.stdlib;
    lib_dir = "./test/typecheck/interfaces";
  }

(** [ir_file_test name ~src ~reference] tests ir generation for [src],
    comparing the resulting files with [reference] *)
let ir_file_test name ~src ~reference =
  let expected = file_contents reference in
  let ir_out = output_file ~ext:"ir.output" src in
  let ir_out_optimized = output_file ~ext:"ir.o.output" src in
  ignore
    (Ir.Diagnostic.file_to_file ~src ~out:ir_out ~deps ~optimize:false
       ());
  ignore
    (Ir.Diagnostic.file_to_file ~src ~out:ir_out_optimized ~deps
       ~optimize:true ());
  let out = output_file src in
  let out_optimized = output_file ~ext:"o.output" src in
  let command =
    Printf.sprintf "./irrun %s > %s\n./irrun %s > %s" ir_out out
      ir_out_optimized out_optimized
  in
  ignore (Sys.command command);
  let actual = file_contents out in
  let actual_optimized = file_contents out_optimized in
  ( (name >:: fun _ -> assert_equal expected actual),
    name ^ " optimized" >:: fun _ ->
    assert_equal expected actual_optimized )

(** [ir_file_tests path] compiles all test cases in [path] using
    [ir_file_test] *)
let ir_file_tests = map2_file_tests_xi ~f:ir_file_test ".irsol.nml"

(** [ir_file_test_cases] is a list of unit tests for ir generation
    files. *)
let ir_file_test_cases =
  List.flatten
    [
      ir_file_tests "./test/irrun";
      ir_file_tests "./test/irrun/autograder";
    ]

let suite =
  "unit test suite for irgen and irrun"
  >::: List.flatten [ ir_file_test_cases ]
