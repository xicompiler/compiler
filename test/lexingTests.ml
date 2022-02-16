open OUnit2
open Parsing
open Parsing.Lexer
open Parsing.Parser

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
  assert_equal (LexerDebug.lex_string input) expected

(** [lexing_test_ok n i e] calls [lexing_test] with [n], [i], and [e]
    mapped as valid tokens. *)
let lexing_test_ok test_name input expected =
  lexing_test test_name input (List.map Result.ok expected)

(** [lexing_test_error n i e] calls [lexing_test] with [n], [i], and [e]
    mapped as invalid tokens. *)
let lexing_test_err test_name input expected =
  lexing_test test_name input (List.map Result.error expected)

(** [file_contents in_file] is a string containing the contents of
    [in_file]. *)
let file_contents in_file =
  let ch = open_in in_file in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

(** [lexing_file_test name ~src ~dst ~reference] constructs an OUnit
    test with name [name] asserting that following
    [lex_to_file ~src ~dst], the contents of [dst] and [reference] are
    equal. *)
let lexing_file_test name ~src ~dst ~reference =
  let expected = file_contents reference in
  LexerDebug.lex_to_file ~src ~dst;
  let actual = file_contents dst in
  name >:: fun _ -> assert_equal expected actual

(* Maps each file in [dir] using [lexing_file_test]. *)
let lexing_file_tests dir =
  let make_test file =
    if Filename.extension file = ".xi" then
      let name =
        file |> Filename.remove_extension |> Printf.sprintf "%s/%s" dir
      in
      let src = name ^ ".xi" in
      let dst = name ^ ".output" in
      let reference = name ^ ".lexedsol" in
      Some (lexing_file_test name ~src ~dst ~reference)
    else None
  in
  Sys.readdir dir |> Array.to_list |> List.filter_map make_test

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
    lexing_test_err "test open string" "\""
      [ { cause = InvalidString; position = { line = 1; column = 1 } } ];
    lexing_test_err "test open char" "'"
      [ { cause = InvalidChar; position = { line = 1; column = 1 } } ];
    lexing_test_err "test invalid unicode" "'\\k'"
      [ { cause = InvalidChar; position = { line = 1; column = 1 } } ];
    lexing_test_err "test invalid unicode" "'\\x{FFFFFF}'"
      [ { cause = InvalidChar; position = { line = 1; column = 1 } } ];
  ]

(** [lexing_test_cases] is a list of unit tests for our test files. *)
let lexing_file_test_cases =
  List.flatten
    [
      lexing_file_tests "./test/lexing/autograder";
      lexing_file_tests "./test/lexing";
    ]

let lexing_suite =
  List.flatten [ lexing_test_cases; lexing_file_test_cases ]

let _ = run_test_tt_main lexing_suite