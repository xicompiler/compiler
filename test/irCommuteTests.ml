open OUnit2
open Ir
open Infix
open Mir

(** [commute_test ~name ~expect e1 e2] constructs an OUnit test with
    name [name] asserting that [commute e1 e2] is equal to [expect] *)
let commute_test ~name ~expect e1 e2 =
  name >:: fun _ ->
  let actual = commute e1 e2 in
  assert_equal expect actual ~printer:Bool.to_string

let t1 = `Temp "t1"
let t2 = `Temp "t2"
let mem = `Mem one
let def_t1 : Mir.expr = `ESeq ((t1 := one), t1)

let suite =
  "commute test suite"
  >::: [
         commute_test ~name:"diff temps" ~expect:true t1 t2;
         commute_test ~name:"modify t1 in e2" ~expect:false t1 def_t1;
         commute_test ~name:"modift t1 in e1" ~expect:true def_t1 t1;
         commute_test ~name:"one mem node" ~expect:true mem t1;
         commute_test ~name:"two mem nodes" ~expect:false mem mem;
       ]
