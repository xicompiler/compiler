open OUnit2
open Frontend

let checking_suite = "unit test suite for lexing" >::: List.flatten []
let _ = run_test_tt_main checking_suite