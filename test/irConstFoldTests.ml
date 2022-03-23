open OUnit2
open Ir
open Core
open Int64

(** [const_fold_test ~name ~expect prog] is an OUnit test case with name
    [name] asserting that the constant folded IR of [prog] is
    structurally equal to [expect] *)
let const_fold_test ~name ~expect prog =
  let reordered = const_fold prog in
  name >:: fun _ -> assert_equal expect reordered

(** [const_fold_same ~name prog] tests that no constant folding occurs
    on [prog] *)
let const_fold_tests tests =
  let f (name, prog, expect) = const_fold_test ~name ~expect prog in
  List.map tests ~f

let arith_tests : (string * Reorder.t * Reorder.t) list =
  [
    ( "mult",
      [
        `Func
          ( "mult",
            [ `Move (`Temp "x", `Bop (`Mult, `Const 2L, `Const 3L)) ] );
      ],
      [ `Func ("mult", [ `Move (`Temp "x", `Const 6L) ]) ] );
    ( "highmult",
      [
        `Func
          ( "highmult",
            [
              `Move
                ( `Temp "x",
                  `Bop (`HighMult, `Const max_value, `Const max_value)
                );
            ] );
      ],
      [
        `Func
          ( "highmult",
            [
              `Move
                ( `Temp "x",
                  `Const (Binop.Arith.high_mult max_value max_value) );
            ] );
      ] );
    ( "highmult zeros",
      [
        `Func
          ( "highmult zeros",
            [
              `Move (`Temp "x", `Bop (`HighMult, `Const 2L, `Const 3L));
            ] );
      ],
      [ `Func ("highmult zeros", [ `Move (`Temp "x", `Const zero) ]) ]
    );
    ( "plus",
      [
        `Func
          ( "plus",
            [ `Move (`Temp "x", `Bop (`Plus, `Const one, `Const one)) ]
          );
      ],
      [ `Func ("plus", [ `Move (`Temp "x", `Const 2L) ]) ] );
    ( "minus",
      [
        `Func
          ( "minus",
            [ `Move (`Temp "x", `Bop (`Minus, `Const 4L, `Const one)) ]
          );
      ],
      [ `Func ("minus", [ `Move (`Temp "x", `Const 3L) ]) ] );
    ( "div",
      [
        `Func
          ( "div",
            [ `Move (`Temp "x", `Bop (`Div, `Const 4L, `Const 2L)) ] );
      ],
      [ `Func ("div", [ `Move (`Temp "x", `Const 2L) ]) ] );
    ( "mod",
      [
        `Func
          ( "mod",
            [ `Move (`Temp "x", `Bop (`Mod, `Const 5L, `Const 3L)) ] );
      ],
      [ `Func ("mod", [ `Move (`Temp "x", `Const 2L) ]) ] );
    ( "error",
      [
        `Func
          ( "error",
            [ `Move (`Temp "x", `Bop (`Div, `Const 2L, `Const zero)) ]
          );
      ],
      [
        `Func
          ( "error",
            [ `Move (`Temp "x", `Bop (`Div, `Const 2L, `Const zero)) ]
          );
      ] );
    ( "nested",
      [
        `Func
          ( "nested",
            [
              `Move
                ( `Temp "x",
                  `Bop
                    ( `Plus,
                      `Bop (`Mult, `Const 2L, `Const 3L),
                      `Bop (`Minus, `Const 4L, `Const one) ) );
            ] );
      ],
      [ `Func ("nested", [ `Move (`Temp "x", `Const 9L) ]) ] );
  ]

let cmp_tests : (string * Reorder.t * Reorder.t) list =
  [
    ( "lt",
      [
        `Func
          ("lt", [ `Move (`Temp "x", `Bop (`Lt, `Const 2L, `Const 3L)) ]);
      ],
      [ `Func ("lt", [ `Move (`Temp "x", `Const one) ]) ] );
    ( "geq",
      [
        `Func
          ( "geq",
            [ `Move (`Temp "x", `Bop (`Geq, `Const 2L, `Const 3L)) ] );
      ],
      [ `Func ("geq", [ `Move (`Temp "x", `Const zero) ]) ] );
    ( "eq",
      [
        `Func
          ( "eq",
            [ `Move (`Temp "x", `Bop (`Div, `Const 2L, `Const 2L)) ] );
      ],
      [ `Func ("eq", [ `Move (`Temp "x", `Const one) ]) ] );
    ( "neq",
      [
        `Func
          ( "neq",
            [ `Move (`Temp "x", `Bop (`Mod, `Const 2L, `Const 2L)) ] );
      ],
      [ `Func ("neq", [ `Move (`Temp "x", `Const zero) ]) ] );
    ( "nested",
      [
        `Func
          ( "nested",
            [
              `Move
                ( `Temp "x",
                  `Bop
                    ( `Neq,
                      `Bop (`Leq, `Const 2L, `Const 2L),
                      `Bop (`Gt, `Const 2L, `Const 2L) ) );
            ] );
      ],
      [ `Func ("nested", [ `Move (`Temp "x", `Const one) ]) ] );
  ]

let log_tests : (string * Reorder.t * Reorder.t) list =
  [
    ( "and",
      [
        `Func
          ( "and",
            [ `Move (`Temp "x", `Bop (`And, `Const zero, `Const one)) ]
          );
      ],
      [ `Func ("and", [ `Move (`Temp "x", `Const zero) ]) ] );
    ( "or",
      [
        `Func
          ( "or",
            [ `Move (`Temp "x", `Bop (`Or, `Const zero, `Const one)) ]
          );
      ],
      [ `Func ("or", [ `Move (`Temp "x", `Const one) ]) ] );
    ( "nested",
      [
        `Func
          ( "nested",
            [
              `Move
                ( `Temp "x",
                  `Bop
                    ( `Or,
                      `Bop (`And, `Const one, `Const one),
                      `Bop (`Or, `Const zero, `Const zero) ) );
            ] );
      ],
      [ `Func ("nested", [ `Move (`Temp "x", `Const one) ]) ] );
  ]

let bitwise_tests : (string * Reorder.t * Reorder.t) list =
  [
    ( "xor 00",
      [
        `Func
          ( "xor",
            [ `Move (`Temp "x", `Bop (`Xor, `Const zero, `Const zero)) ]
          );
      ],
      [ `Func ("xor", [ `Move (`Temp "x", `Const zero) ]) ] );
    ( "xor 01",
      [
        `Func
          ( "xor",
            [ `Move (`Temp "x", `Bop (`Xor, `Const zero, `Const one)) ]
          );
      ],
      [ `Func ("xor", [ `Move (`Temp "x", `Const one) ]) ] );
    ( "xor 00 11",
      [
        `Func
          ( "xor",
            [ `Move (`Temp "x", `Bop (`Xor, `Const zero, `Const 3L)) ]
          );
      ],
      [ `Func ("xor", [ `Move (`Temp "x", `Const 3L) ]) ] );
  ]

let unsigned_tests : (string * Reorder.t * Reorder.t) list =
  [
    ( "ult -1 0",
      [
        `Func
          ( "ult",
            [ `Move (`Temp "x", `Bop (`ULt, `Const ~-1L, `Const zero)) ]
          );
      ],
      [ `Func ("ult", [ `Move (`Temp "x", `Const zero) ]) ] );
    ( "ult 1 -2",
      [
        `Func
          ( "ult",
            [ `Move (`Temp "x", `Bop (`ULt, `Const 1L, `Const ~-2L)) ]
          );
      ],
      [ `Func ("ult", [ `Move (`Temp "x", `Const one) ]) ] );
    ( "ult 1 2",
      [
        `Func
          ( "ult",
            [ `Move (`Temp "x", `Bop (`ULt, `Const 1L, `Const 2L)) ] );
      ],
      [ `Func ("ult", [ `Move (`Temp "x", `Const one) ]) ] );
    ( "ult -1 -2",
      [
        `Func
          ( "ult",
            [ `Move (`Temp "x", `Bop (`ULt, `Const ~-1L, `Const ~-2L)) ]
          );
      ],
      [ `Func ("ult", [ `Move (`Temp "x", `Const zero) ]) ] );
  ]

let arith_test_cases = const_fold_tests arith_tests

let cmp_test_cases = const_fold_tests cmp_tests

let log_test_cases = const_fold_tests log_tests

let bitwise_test_cases = const_fold_tests bitwise_tests

let unsigned_test_cases = const_fold_tests unsigned_tests

let suite =
  "ir const fold test suite"
  >::: List.concat
         [
           arith_test_cases;
           cmp_test_cases;
           log_test_cases;
           bitwise_test_cases;
           unsigned_test_cases;
         ]
