open Core
module CFG = Graph.Directed.IntDigraph

let find_label vs =
  let map = String.Table.create () in
  List.iter vs ~f:(fun v ->
      match CFG.Vertex.value v with
      | `Label l -> Hashtbl.add_exn map ~key:l ~data:v
      | _ -> ());
  Hashtbl.find_exn map

let add_jump src l ~label =
  let dst = label l in
  CFG.Vertex.add_unweighted_edge ~src ~dst

let add_fallthrough src = function
  | [] -> ()
  | dst :: _ -> CFG.Vertex.add_unweighted_edge ~src ~dst

let add_edge ~label src vs =
  match CFG.Vertex.value src with
  | `Jump (`Name l) -> add_jump src l ~label
  | `Return _ | `Jump _ -> ()
  | `CJump (_, t, f) ->
      add_jump src t ~label;
      add_jump src f ~label
  | #Lir.stmt -> add_fallthrough src vs

let rec add_edges ~label = function
  | [] -> ()
  | v :: vs ->
      add_edge ~label v vs;
      add_edges ~label vs

let create_cfg stmts =
  let vs = CFG.indexed stmts in
  let label = find_label vs in
  add_edges ~label vs;
  vs
