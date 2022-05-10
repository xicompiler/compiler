open OUnit2

let suite =
  "unit test suite for compiler"
  >::: [
         LexingTests.suite;
         ParsingTests.suite;
         TypeCheckingTests.suite;
         MangleTests.suite;
         ReorderTests.suite;
         IrConstFoldTests.suite;
         IrCommuteTests.suite;
         IrRunTests.suite;
         AssemblyTests.suite;
         BitSetTests.suite;
       ]

let _ = run_test_tt_main suite
