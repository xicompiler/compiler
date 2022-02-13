open OUnit2
open ParsingTests
open LexingTests

let suite =
  "unit test suite for compiler"
  >::: List.flatten
         [ ParsingTests.parsing_suite; LexingTests.lexing_suite ]

let _ = run_test_tt_main suite