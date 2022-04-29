open OUnit2
open Ir
open Core
open Int64

(** [const_fold_test ~name ~expect prog] is an OUnit test case with name
    [name] asserting that the constant folded IR of [prog] is
    structurally equal to [expect] *)
let const_fold_test ~name ~expect prog =
  name >:: fun _ -> assert_equal expect (ConstFold.const_fold prog)

(** [const_fold_same ~name prog] tests that no constant folding occurs
    on [prog] *)
let const_fold_tests tests =
  let f (name, prog, expect) = const_fold_test ~name ~expect prog in
  List.map tests ~f

(** [func name stmts] is the simple IR representation of a function with
    [name] and [stmts] *)
let func name stmts = `Func (name, stmts, 0, 0)

let arith_tests : (string * Reorder.t * Reorder.t) list =
  [
    ( "mult",
      [
        func "mult"
          [ `Move (`Temp "x", `Bop (`Mul, `Const 2L, `Const 3L)) ];
      ],
      [ func "mult" [ `Move (`Temp "x", `Const 6L) ] ] );
    ( "highmult",
      [
        func "highmult"
          [
            `Move
              ( `Temp "x",
                `Bop (`HMul, `Const max_value, `Const max_value) );
          ];
      ],
      [
        func "highmult"
          [
            `Move
              ( `Temp "x",
                `Const (Binop.Arith.high_mult max_value max_value) );
          ];
      ] );
    ( "highmult zeros",
      [
        func "highmult zeros"
          [ `Move (`Temp "x", `Bop (`HMul, `Const 2L, `Const 3L)) ];
      ],
      [ func "highmult zeros" [ `Move (`Temp "x", `Const zero) ] ] );
    ( "plus",
      [
        func "plus"
          [ `Move (`Temp "x", `Bop (`Add, `Const one, `Const one)) ];
      ],
      [ func "plus" [ `Move (`Temp "x", `Const 2L) ] ] );
    ( "minus",
      [
        func "minus"
          [ `Move (`Temp "x", `Bop (`Sub, `Const 4L, `Const one)) ];
      ],
      [ func "minus" [ `Move (`Temp "x", `Const 3L) ] ] );
    ( "div",
      [
        func "div"
          [ `Move (`Temp "x", `Bop (`Div, `Const 4L, `Const 2L)) ];
      ],
      [ func "div" [ `Move (`Temp "x", `Const 2L) ] ] );
    ( "mod",
      [
        func "mod"
          [ `Move (`Temp "x", `Bop (`Mod, `Const 5L, `Const 3L)) ];
      ],
      [ func "mod" [ `Move (`Temp "x", `Const 2L) ] ] );
    ( "error",
      [
        func "error"
          [ `Move (`Temp "x", `Bop (`Div, `Const 2L, `Const zero)) ];
      ],
      [
        func "error"
          [ `Move (`Temp "x", `Bop (`Div, `Const 2L, `Const zero)) ];
      ] );
    ( "nested",
      [
        func "nested"
          [
            `Move
              ( `Temp "x",
                `Bop
                  ( `Add,
                    `Bop (`Mul, `Const 2L, `Const 3L),
                    `Bop (`Sub, `Const 4L, `Const one) ) );
          ];
      ],
      [ func "nested" [ `Move (`Temp "x", `Const 9L) ] ] );
  ]

let cmp_tests : (string * Reorder.t * Reorder.t) list =
  [
    ( "lt",
      [
        func "lt"
          [ `Move (`Temp "x", `Bop (`Lt, `Const 2L, `Const 3L)) ];
      ],
      [ func "lt" [ `Move (`Temp "x", `Const one) ] ] );
    ( "geq",
      [
        func "geq"
          [ `Move (`Temp "x", `Bop (`Geq, `Const 2L, `Const 3L)) ];
      ],
      [ func "geq" [ `Move (`Temp "x", `Const zero) ] ] );
    ( "eq",
      [
        func "eq"
          [ `Move (`Temp "x", `Bop (`Div, `Const 2L, `Const 2L)) ];
      ],
      [ func "eq" [ `Move (`Temp "x", `Const one) ] ] );
    ( "neq",
      [
        func "neq"
          [ `Move (`Temp "x", `Bop (`Mod, `Const 2L, `Const 2L)) ];
      ],
      [ func "neq" [ `Move (`Temp "x", `Const zero) ] ] );
    ( "nested",
      [
        func "nested"
          [
            `Move
              ( `Temp "x",
                `Bop
                  ( `Neq,
                    `Bop (`Leq, `Const 2L, `Const 2L),
                    `Bop (`Gt, `Const 2L, `Const 2L) ) );
          ];
      ],
      [ func "nested" [ `Move (`Temp "x", `Const one) ] ] );
  ]

let log_tests : (string * Reorder.t * Reorder.t) list =
  [
    ( "and",
      [
        func "and"
          [ `Move (`Temp "x", `Bop (`And, `Const zero, `Const one)) ];
      ],
      [ func "and" [ `Move (`Temp "x", `Const zero) ] ] );
    ( "or",
      [
        func "or"
          [ `Move (`Temp "x", `Bop (`Or, `Const zero, `Const one)) ];
      ],
      [ func "or" [ `Move (`Temp "x", `Const one) ] ] );
    ( "nested",
      [
        func "nested"
          [
            `Move
              ( `Temp "x",
                `Bop
                  ( `Or,
                    `Bop (`And, `Const one, `Const one),
                    `Bop (`Or, `Const zero, `Const zero) ) );
          ];
      ],
      [ func "nested" [ `Move (`Temp "x", `Const one) ] ] );
  ]

let bitwise_tests : (string * Reorder.t * Reorder.t) list =
  [
    ( "xor 00",
      [
        func "xor"
          [ `Move (`Temp "x", `Bop (`Xor, `Const zero, `Const zero)) ];
      ],
      [ func "xor" [ `Move (`Temp "x", `Const zero) ] ] );
    ( "xor 01",
      [
        func "xor"
          [ `Move (`Temp "x", `Bop (`Xor, `Const zero, `Const one)) ];
      ],
      [ func "xor" [ `Move (`Temp "x", `Const one) ] ] );
    ( "xor 00 11",
      [
        func "xor"
          [ `Move (`Temp "x", `Bop (`Xor, `Const zero, `Const 3L)) ];
      ],
      [ func "xor" [ `Move (`Temp "x", `Const 3L) ] ] );
    ( "nested double negation",
      [
        func "xor"
          [
            `Move
              ( `Temp "x",
                `Bop
                  (`Xor, `Bop (`Xor, `Temp "y", `Const one), `Const one)
              );
          ];
      ],
      [ func "xor" [ `Move (`Temp "x", `Temp "y") ] ] );
    ( "nested triple negation",
      [
        func "xor"
          [
            `Move
              ( `Temp "x",
                `Bop
                  ( `Xor,
                    `Bop
                      ( `Xor,
                        `Bop (`Xor, `Temp "y", `Const one),
                        `Const one ),
                    `Const one ) );
          ];
      ],
      [
        func "xor"
          [ `Move (`Temp "x", `Bop (`Xor, `Temp "y", `Const one)) ];
      ] );
  ]

let unsigned_tests : (string * Reorder.t * Reorder.t) list =
  [
    ( "ult -1 0",
      [
        func "ult"
          [ `Move (`Temp "x", `Bop (`ULt, `Const ~-1L, `Const zero)) ];
      ],
      [ func "ult" [ `Move (`Temp "x", `Const zero) ] ] );
    ( "ult 1 -2",
      [
        func "ult"
          [ `Move (`Temp "x", `Bop (`ULt, `Const 1L, `Const ~-2L)) ];
      ],
      [ func "ult" [ `Move (`Temp "x", `Const one) ] ] );
    ( "ult 1 2",
      [
        func "ult"
          [ `Move (`Temp "x", `Bop (`ULt, `Const 1L, `Const 2L)) ];
      ],
      [ func "ult" [ `Move (`Temp "x", `Const one) ] ] );
    ( "ult -1 -2",
      [
        func "ult"
          [ `Move (`Temp "x", `Bop (`ULt, `Const ~-1L, `Const ~-2L)) ];
      ],
      [ func "ult" [ `Move (`Temp "x", `Const zero) ] ] );
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
