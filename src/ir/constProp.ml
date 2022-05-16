open Core

type defined =
  | Top
  | Def of int64
  | Bottom
[@@deriving hash, compare, sexp, equal]

module Analysis = struct
  let combine d1 d2 =
    match (d1, d2) with
    | Top, Top -> Top
    | Bottom, _ | _, Bottom | Top, _ | _, Top -> Bottom
    | Def x, Def y -> if Int64.equal x y then Def x else Bottom

  let equal d1 d2 =
    match (d1, d2) with
    | Bottom, Bottom -> true
    | Top, Top -> true
    | Def x, Def y -> Int64.equal x y
    | _, _ -> false

  let meet = function
    | h :: t ->
        let combine ~key = combine in
        List.fold ~f:(Map.merge_skewed ~combine) ~init:h t
    | [] -> Temp.Virtual.Map.empty

  let top stmts =
    let temps =
      List.fold
        ~f:(fun acc x -> Lir.def ~init:(Lir.use ~init:acc x) x)
        ~init:Temp.Virtual.Set.empty stmts
    in
    Set.fold temps ~init:Temp.Virtual.Map.empty ~f:(fun acc x ->
        Map.set acc ~key:x ~data:Top)

  let params ~gen ~stmts =
    let f ~data ~vertex =
      match gen data vertex with
      | Some (t, v) -> Map.set data ~key:t ~data:v
      | None -> data
    in
    Dataflow.Params.
      {
        f;
        meet;
        top = top stmts;
        direction = `Forward;
        equal = Map.equal equal;
      }
end

let rec gen_expr ~map = function
  | `Const i -> Def i
  | `Name _ | `Mem _ -> Bottom
  | `Bop (op, e1, e2) -> gen_expr_bop ~map op e1 e2
  | #Temp.Virtual.t as e -> begin
      match Map.find map e with Some const -> const | None -> Bottom
    end

and gen_expr_bop ~map op e1 e2 =
  match (gen_expr map e1, gen_expr map e2) with
  | Top, Top -> Top
  | Bottom, _ | _, Bottom | Top, _ | _, Top -> Bottom
  | Def x, Def y -> begin
      match Op.eval op x y with Some n -> Def n | _ -> Bottom
    end

let gen map = function
  | `Move ((`Temp t as temp), e) -> begin
      match gen_expr ~map e with
      | Def n -> Some (temp, Def n)
      | Bottom -> Some (temp, Bottom)
      | Top -> Some (temp, Top)
    end
  | #Lir.stmt -> None

let analyze cfg stmts =
  let params = Analysis.params ~gen ~stmts in
  Lir.CFG.analyze cfg params

let const_in stmts =
  let vs = Lir.create_cfg stmts in
  let cfg = Lir.CFG.of_vertices vs in
  let const = analyze cfg stmts in
  fun i -> (const i).input

let find_temp ~map temp =
  match Map.find map temp with
  | Some const -> (
      match const with
      | Bottom | Top -> (temp :> Lir.expr)
      | Def n -> `Const n)
  | None -> (temp :> Lir.expr)

let propagate_stmts stmts =
  let consts = const_in stmts in
  let f i = Lir.map_stmt ~f:(find_temp ~map:(consts i)) in
  List.mapi ~f stmts

let propagate_toplevel : Lir.toplevel -> Lir.toplevel = function
  | `Data _ as d -> d
  | `Func (l, b, a, r) -> `Func (l, propagate_stmts b, a, r)

let propagate = List.map ~f:propagate_toplevel
