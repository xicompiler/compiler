open OUnit2
open LexingTests

let suite =
  "unit test suite for compiler"
  >::: List.flatten [ LexingTests.lexing_suite ]

let _ = run_test_tt_main suite