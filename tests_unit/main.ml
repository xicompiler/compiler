open OUnit2
open Parsing

let parsing_tests = [
]

let suite =
  "unit test suite for compiler"
  >::: List.flatten
         [ parsing_tests; ]

let _ = run_test_tt_main suite