open Core
open OUnit2
open Util.BitSet

(** [set_equal_test ~name ~expected set] is an test with name [name]
    asserting that that [expected] and the elements of [set] contain the
    same elements *)
let set_equal_test ~name ~expected set =
  let real = List.sort ~compare:Int.compare (elements set) in
  let printer = List.to_string ~f:Int.to_string in
  name >:: fun _ -> assert_equal real expected ~printer

let empty_test = set_equal_test ~name:"empty" ~expected:[] empty

let universe_test =
  let expected = List.range 0 63 in
  set_equal_test ~name:"universe" ~expected universe

(** [s1] is the set [{1, 2}] *)
let s1 = empty |> add 1 |> add 2

(** [s2] is the set [{2, 3}] *)
let s2 = empty |> add 2 |> add 3

let union_test =
  set_equal_test ~name:"union" ~expected:[ 1; 2; 3 ] (union s1 s2)

let inter_test =
  set_equal_test ~name:"inter" ~expected:[ 2 ] (inter s1 s2)

let diff_test = set_equal_test ~name:"diff" ~expected:[ 1 ] (diff s1 s2)

let suite =
  "BitSet test suite"
  >::: [ empty_test; universe_test; union_test; inter_test ]
