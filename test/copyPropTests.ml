open Core
open OUnit2
open Ir.CopyProp
open OUnit2
open Ir
open Core
open Int64

(** [copy_prop_test ~name ~expect prog] is an OUnit test case with name
    [name] asserting that the copy propagated IR of [prog] is
    structurally equal to [expect] *)
let copy_prop_test ~name ~expect prog =
  name >:: fun _ -> assert_equal expect (CopyProp.propagate prog)

(** [copy_prop_tests tests] tests copy propagation on [tests] *)
let copy_prop_tests tests =
  let f (name, prog, expect) = copy_prop_test ~name ~expect prog in
  List.map tests ~f

(** [func name stmts] is the simple IR representation of a function with
    [name] and [stmts] *)
let func name stmts = `Func (name, stmts, 0, 0)

let simple_tests : (string * Lir.t * Lir.t) list =
  [
    ( "same xy then yz",
      [
        func "same"
          [ `Move (`Temp "x", `Temp "y"); `Move (`Temp "y", `Temp "z") ];
      ],
      [
        func "same"
          [ `Move (`Temp "x", `Temp "y"); `Move (`Temp "y", `Temp "z") ];
      ] );
    ( "basic xy then zx",
      [
        func "basic"
          [ `Move (`Temp "x", `Temp "y"); `Move (`Temp "z", `Temp "x") ];
      ],
      [
        func "basic"
          [ `Move (`Temp "x", `Temp "y"); `Move (`Temp "z", `Temp "y") ];
      ] );
    ( "cascade cd then bc then ab",
      [
        func "cascade"
          [
            `Move (`Temp "c", `Temp "d");
            `Move (`Temp "b", `Temp "c");
            `Move (`Temp "a", `Temp "b");
          ];
      ],
      [
        func "cascade"
          [
            `Move (`Temp "c", `Temp "d");
            `Move (`Temp "b", `Temp "d");
            `Move (`Temp "a", `Temp "c");
          ];
      ] );
  ]

let simple = copy_prop_tests simple_tests
let suite = "copy propagation test suite" >::: List.concat [ simple ]
