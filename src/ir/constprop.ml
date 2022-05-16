open Core

type defined =
  | Top
  | Def of int64
  | Bottom
[@@deriving hash, compare, sexp, equal]

module ConstProp = struct
  let defined_combine d1 d2 =
    match (d1, d2) with
    | Bottom, _ | _, Bottom -> Bottom
    | Top, _ | _, Top -> Top
    | Def x, Def y -> if Int64.equal x y then Def x else Bottom

  let defined_equal d1 d2 =
    match (d1, d2) with
    | Bottom, Bottom -> true
    | Top, Top -> true
    | Def x, Def y -> Int64.equal x y
    | _, _ -> false

  let meet = function
    | h :: t ->
        let combine ~key = defined_combine in
        List.fold ~f:(Map.merge_skewed ~combine) ~init:h t
    | [] -> Temp.Virtual.Map.empty

  let top stmts =
    let temps =
      List.fold
        ~f:(fun acc x -> Lir.def ~init:(Lir.use ~init:acc x) x)
        ~init:Temp.Virtual.Set.empty stmts
    in
    Set.fold temps ~init:Temp.Virtual.Map.empty ~f:(fun acc x ->
        Map.add_exn acc ~key:x ~data:Top)

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
        equal = Map.equal defined_equal;
      }
end

let rec gen_expr map = function
  | `Const i -> Def i
  | `Name _ | `Mem _ -> Bottom
  | `Bop (op, e1, e2) -> gen_expr_bop op e1 e2 map
  | #Temp.Virtual.t as e -> Map.find_exn map e

and gen_expr_bop op e1 e2 map =
  match (gen_expr map e1, gen_expr map e2) with
  | Bottom, _ | _, Bottom -> Bottom
  | Top, _ | _, Top -> Top
  | Def x, Def y -> begin
      match Op.eval op x y with Some n -> Def n | _ -> Bottom
    end

let gen map = function
  | `Move ((`Temp t as temp), e) -> begin
      match gen_expr map e with
      | Def n -> Some (temp, Def n)
      | Bottom -> Some (temp, Bottom)
      | Top -> Some (temp, Top)
    end
  | #Lir.stmt -> None

let analyze cfg stmts =
  let params = ConstProp.params ~gen ~stmts in
  Lir.CFG.analyze cfg params

let const_out stmts =
  let vs = Lir.create_cfg stmts in
  let cfg = Lir.CFG.of_vertices vs in
  let const = analyze cfg stmts in
  fun i -> (const i).output

let find_temp ~map temp =
  match Map.find_exn map temp with
  | Bottom | Top -> (temp :> Lir.expr)
  | Def n -> (`Const n :> Lir.expr)

let rec sub_expr ~map = function
  | #Temp.Virtual.t as t -> find_temp ~map t
  | `Bop (op, e1, e2) -> `Bop (op, sub_expr ~map e1, sub_expr ~map e2)
  | #Lir.expr as e -> e

let sub_exprs ~map = List.map ~f:(sub_expr ~map)

let propagate_stmts stmts =
  let const = const_out stmts in
  let f i =
    let map = const i in
    function
    | `Call (n, e, es) -> `Call (n, sub_expr ~map e, sub_exprs ~map es)
    | `CJump (e, l1, l2) -> `CJump (sub_expr ~map e, l1, l2)
    | `Jump e -> `Jump (sub_expr ~map e)
    | `Return es -> `Return (sub_exprs ~map es)
    | `Move ((`Temp _ as t), e) -> `Move (t, sub_expr ~map e)
    | #Lir.stmt as s -> s
  in
  List.mapi ~f stmts

let propagate_toplevel : Lir.toplevel -> Lir.toplevel = function
  | `Data _ as d -> d
  | `Func (l, b, a, r) -> `Func (l, propagate_stmts b, a, r)

let propagate = List.map ~f:propagate_toplevel
