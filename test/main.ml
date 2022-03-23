open OUnit2

let suite =
  "unit test suite for compiler"
  >::: [
         (* LexingTests.suite; ParsingTests.suite;
            TypeCheckingTests.suite; ReorderTests.suite; *)
         IrConstFoldTests.suite;
       ]

let _ = run_test_tt_main suite