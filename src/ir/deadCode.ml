open Core
module Liveness = Dataflow.Liveness.Make (Temp.Virtual.Set)

let params = Liveness.params ~use:Lir.use ~def:Lir.def

let live_out stmts =
  let vs = Lir.create_cfg stmts in
  let cfg = Lir.CFG.of_vertices vs in
  let live = Lir.CFG.analyze cfg params in
  fun i -> (live i).input

(** [eliminate_stmts stmts] is [stmts] with dead definitions removed *)
let eliminate_stmts stmts =
  let live = live_out stmts in
  let f i = function
    | `Move ((`Temp _ as t), _) -> Set.mem (live i) t
    | #Lir.stmt -> true
  in
  List.filteri ~f stmts

let eliminate_toplevel : Lir.toplevel -> Lir.toplevel = function
  | `Data _ as d -> d
  | `Func (l, b, a, r) -> `Func (l, eliminate_stmts b, a, r)

let eliminate = List.map ~f:eliminate_toplevel
