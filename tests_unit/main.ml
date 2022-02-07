open OUnit2
open Parsing


let make_parsing_test test_name input expected = 
  test_name >:: (fun _ -> assert_equal (Lexer.lex_string input) expected)

let make_parsing_test_ok test_name input expected_tokens =
  make_parsing_test test_name input (List.map Result.ok expected_tokens)

let parsing_tests = [
  make_parsing_test_ok "test string hi" "\"hi\"" [Parser.STRING "hi"];
  make_parsing_test_ok "test unicode" "\"\\x{190}\"" [Parser.STRING "Ɛ"];
]

let suite =
  "unit test suite for compiler"
  >::: List.flatten
         [ parsing_tests; ]

let _ = run_test_tt_main suite