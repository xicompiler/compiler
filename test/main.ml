open OUnit2

let suite =
  "unit test suite for compiler"
  >::: [ LexingTests.lexing_suite; ParsingTests.parsing_suite ]

let _ = run_test_tt_main suite