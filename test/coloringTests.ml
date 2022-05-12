open Core
open OUnit2
module G = Graph.Undirected.Make (Int)

(** The graph in the course notes *)
let feasible =
  G.of_edges [ (1, 2); (1, 3); (2, 3); (2, 4); (3, 4); (3, 5); (4, 5) ]

let coloring = G.kempe feasible ~max:3

let infeasible =
  G.of_edges [ (1, 2); (1, 3); (1, 4); (2, 3); (2, 4); (3, 4) ]

let spills = G.kempe infeasible ~max:3

let suite =
  "coloring test suite"
  >::: [
         ("feasible" >:: fun _ -> assert (Result.is_ok coloring));
         ("infeasible" >:: fun _ -> assert (Result.is_error spills));
       ]
