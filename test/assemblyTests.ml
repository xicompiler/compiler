open OUnit2
open Common

(** [deps] are the dependencies used for testing *)
let deps : Frontend.Check.dependencies =
  {
    std_dir = Util.File.stdlib;
    lib_dir = "./test/typecheck/interfaces";
  }

let enabled_opt = Opt.enabled Opt.disabled
let disabled_opt = Opt.disabled

(** [asm_file_test name ~src ~reference] tests asm generation for [src],
    comparing the resulting files with [reference] *)
let asm_file_test name ~src ~reference =
  let chopped = output_file ~ext:"sout" src in
  let chopped_o = output_file ~ext:"o.sout" src in
  let expected = file_contents reference in
  let asm_out = output_file ~ext:"s" src in
  let asm_out_optimized = output_file ~ext:"o.s" src in
  print_endline asm_out;
  ignore
    (Instr.Output.file_to_file ~src ~out:asm_out ~deps ~opt:disabled_opt
       ());
  ignore
    (Instr.Output.file_to_file ~src ~out:asm_out_optimized ~deps
       ~opt:enabled_opt ());
  let out = output_file src in
  let out_optimized = output_file ~ext:"o.output" src in
  let command =
    Printf.sprintf
      "./runtime/linkxi.sh %s -o %s\n\
       %s > %s\n\
       ./runtime/linkxi.sh %s -o %s\n\
       %s > %s" asm_out chopped chopped out asm_out_optimized chopped_o
      chopped_o out_optimized
  in
  ignore (Sys.command command);
  let actual = file_contents out in
  let actual_optimized = file_contents out_optimized in
  ( (name >:: fun _ -> assert_equal expected actual),
    name ^ " optimized" >:: fun _ ->
    assert_equal expected actual_optimized )

(** [asm_file_tests path] compiles all test cases in [path] using
    [asm_file_test] *)
let asm_file_tests = map2_file_tests_xi ~f:asm_file_test ".ssol.nml"

(** [asm_file_test_cases] is a list of unit tests for assembly
    generation files. *)
let asm_file_test_cases =
  List.flatten
    [
      asm_file_tests "./test/asm";
      asm_file_tests "./test/asm/autograder";
      asm_file_tests "./test/asm/benchmark";
    ]

let suite =
  "unit test suite for assembly"
  >::: List.flatten [ asm_file_test_cases ]
