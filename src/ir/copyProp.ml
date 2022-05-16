open Core

module Analysis = struct
  (** [meet data] is the meet of [data] *)
  let meet = function
    | h :: t ->
        let combine ~key = Set.inter in
        List.fold ~f:(Map.merge_skewed ~combine) ~init:h t
    | [] -> Temp.Virtual.Map.empty

  (** [top stmts] is the top values for [stmts] *)
  let top stmts =
    let temps =
      List.fold
        ~f:(fun acc x -> Lir.def ~init:(Lir.use ~init:acc x) x)
        ~init:Temp.Virtual.Set.empty stmts
    in
    let f acc x = Map.set acc ~key:x ~data:temps in
    Set.fold ~f ~init:Temp.Virtual.Map.empty temps

  let diff ~map =
    let f acc t =
      let acc = Map.set acc ~key:t ~data:Temp.Virtual.Set.empty in
      Map.map acc ~f:(fun s -> Set.remove s t)
    in
    Set.fold ~f ~init:map

  let add ~map = function
    | Some (t1, t2) ->
        let current = Map.find_exn map t1 in
        Map.set map ~key:t1 ~data:(Set.add current t2)
    | None -> map

  let params ~gen ~kill ~stmts =
    (* gen[n] = use[n] ∪ (in[n] - kill[n]) *)
    let f ~data ~vertex =
      let survivors = diff ~map:data (kill vertex) in
      add ~map:survivors (gen vertex)
    in
    Dataflow.Params.
      {
        f;
        meet;
        top = top stmts;
        direction = `Forward;
        equal = Map.equal Set.equal;
      }
end

let gen = function
  | `Move ((#Temp.t as t1), (#Temp.t as t2)) -> Some (t1, t2)
  | _ -> None

let analyze stmts =
  let vs = Lir.create_cfg stmts in
  let cfg = Lir.CFG.of_vertices vs in
  let params = Analysis.params ~gen ~kill:Lir.def ~stmts in
  Lir.CFG.analyze cfg params

let copies_in stmts =
  let copies = analyze stmts in
  fun i -> (copies i).input

let choose_temp ~set temp =
  match Set.choose set with Some t -> t | None -> temp

let find_temp ~map temp =
  match Map.find map temp with
  | Some set -> choose_temp ~set temp
  | None -> temp

let propagate_stmts stmts =
  let copies = copies_in stmts in
  let f i = Lir.map_stmt ~f:(find_temp ~map:(copies i)) in
  List.mapi ~f stmts

let propagate_toplevel : Lir.toplevel -> Lir.toplevel = function
  | `Data _ as d -> d
  | `Func (l, b, a, r) -> `Func (l, propagate_stmts b, a, r)

let propagate = List.map ~f:propagate_toplevel
