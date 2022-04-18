open OUnit2
open Ir

(** [gensym ()] is a dummy label *)
let gensym () = "dummy"

(** [reorder_test ~name ~expect prog] is an OUnit test case with name
    [name] asserting that the reordered IR of [prog] is structurally
    equal to [expect] *)
let reorder_test ~name ~expect prog =
  let reordered = Reorder.reorder prog ~gensym in
  name >:: fun _ -> assert_equal expect reordered

(** [func name stmts] is the simple IR representation of a function with
    [name] and [stmts] *)
let func name stmts = `Func (name, stmts, 0, 0)

(** [prog] is the sequence of LIR statements provided in the course
    notes *)
let prog : Lir.t =
  [
    func "a"
      [
        `Label "L0";
        `CJump (`Temp "e", "L2", "L3");
        `Label "L1";
        `Move (`Temp "x", `Temp "y");
        `Label "L2";
        `Move (`Temp "x", `Bop (`Add, `Temp "x", `Temp "y"));
        `Jump (`Name "L1");
        `Label "L3";
        `Call (1, `Name "f", [ `Temp "x" ]);
        `Return [];
      ];
  ]

(** [reordered] is the reordered IR of [prog] *)
let reordered : Reorder.t =
  [
    func "a"
      [
        `CJump (Lir.log_neg (`Temp "e"), "L3");
        `Label "L2";
        `Move (`Temp "x", `Bop (`Add, `Temp "x", `Temp "y"));
        `Move (`Temp "x", `Temp "y");
        `Jump (`Name "L2");
        `Label "L3";
        `Call (1, `Name "f", [ `Temp "x" ]);
        `Return [];
      ];
  ]

let suite =
  "reorder test suite"
  >::: [ reorder_test ~name:"course notes" ~expect:reordered prog ]
