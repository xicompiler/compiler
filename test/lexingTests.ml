open OUnit2
open Frontend
open Lex
open Error
open Parse
open Common

(** [char_token_of_int i] is a [CHAR] token carrying a utf8 codepoint
    with code [i]. *)
let char_token_of_int i = CHAR (Uchar.of_int i)

(** [char_token_of_char c] is a [CHAR] token carrying a utf8 codepoint
    representing character [c]. *)
let char_token_of_char c = CHAR (Uchar.of_char c)

(** [lexing_test s i e] binds [n] to a unit test that asserts [i] and
    [e] are equal. *)
let lexing_test test_name input expected =
  test_name >:: fun _ ->
  assert_equal (Lex.Diagnostic.lex_string input) expected

(** [lexing_test_ok n i e] calls [lexing_test] with [n], [i], and [e]
    mapped as valid tokens. *)
let lexing_test_ok test_name input expected =
  lexing_test test_name input (List.map Result.ok expected)

(** [lexing_test_error n i e] calls [lexing_test] with [n], [i], and [e]
    mapped as invalid tokens. *)
let lexing_test_err test_name input expected =
  lexing_test test_name input (List.map Result.error expected)

(** [lexing_file_test name ~src ~reference] tests lexing for [src],
    comparing the resulting file with [reference] *)
let lexing_file_test name ~src ~reference =
  let expected = file_contents reference in
  let out = output_file src in
  ignore (Lex.Diagnostic.file_to_file ~src ~out);
  let actual = file_contents out in
  name >:: fun _ -> assert_equal expected actual

(* Maps each file in [dir] using [lexing_file_test]. *)
let lexing_file_tests = map_file_tests ~f:lexing_file_test ".lexedsol"

let str_error =
  Position.Error.create ~pos:{ line = 1; column = 1 } InvalidString

let char_error =
  Position.Error.create ~pos:{ line = 1; column = 1 } InvalidChar

(** [lexing_test_cases] is a list of unit tests for [lex_string]. *)
let lexing_test_cases =
  [
    lexing_test_ok "test string hello world" "\"hello world\""
      [ STRING "hello world" ];
    lexing_test_ok "test unicode" "\"\\x{190}\"" [ STRING "Ɛ" ];
    lexing_test_ok "test newline" "'\\n' '\\x{0a}\'"
      [ char_token_of_char '\n'; char_token_of_int 0x0a ];
    lexing_test_ok "test tab" "'\\t' '\\x{9}'"
      [ char_token_of_char '\t'; char_token_of_int 9 ];
    lexing_test_ok "test return" "'\\r' '\\x{D}'"
      [ char_token_of_char '\r'; char_token_of_int 0xd ];
    lexing_test_ok "test backslash" "'\\\\'" [ char_token_of_char '\\' ];
    lexing_test_ok "test backslash unicode" "'\\x{5C}'"
      [ char_token_of_int 0x5c ];
    lexing_test_ok "test quote" "'\"' '\\x{22}'"
      [ char_token_of_char '\"'; char_token_of_int 0x22 ];
    lexing_test_err "test open string" "\"" [ str_error ];
    lexing_test_err "test open char" "'" [ char_error ];
    lexing_test_err "test invalid unicode" "'\\k'" [ char_error ];
    lexing_test_err "test invalid unicode" "'\\x{FFFFFF}'"
      [ char_error ];
  ]

(** [lexing_test_cases] is a list of unit tests for our test files. *)
let lexing_file_test_cases =
  List.flatten
    [
      lexing_file_tests "./test/lexing/autograder";
      lexing_file_tests "./test/lexing";
    ]

let suite =
  "unit test suite for lexing"
  >::: List.flatten [ lexing_test_cases; lexing_file_test_cases ]
