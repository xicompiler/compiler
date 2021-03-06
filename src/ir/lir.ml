open Core
open Subtype
open Infix

type expr = expr Subtype.expr
type stmt = expr Subtype.cjump2
type dest = expr Subtype.dest

type toplevel =
  [ `Func of label * stmt list * int * int
  | `Data of label * int64 list
  ]

let log_neg = Subtype.log_neg

type t = toplevel list

(** [string_of_expr expr] is the string representation of [expr] *)
let rec string_of_expr = function
  | `Name s -> "Name " ^ s
  | `Const i -> Int64.to_string i
  | `Bop (op, e1, e2) ->
      Printf.sprintf "(%s) %s (%s)" (string_of_expr e1)
        (Op.to_string op) (string_of_expr e2)
  | `Temp t -> t
  | `Rv i -> "RV" ^ string_of_int i
  | `Arg i -> "ARG" ^ string_of_int i
  | `Mem e -> Printf.sprintf "[%s]" (string_of_expr e)

(** [string_of_exprs exprs] is the string representation of [exprs] *)
let string_of_exprs es =
  String.concat ~sep:", " (List.map ~f:string_of_expr es)

let to_string : stmt -> string = function
  | `Call (_, e, es) ->
      Printf.sprintf "Call(%s)" (string_of_exprs (e :: es))
  | `Move (e1, e2) ->
      Printf.sprintf "%s <- %s"
        (string_of_expr (e1 :> expr))
        (string_of_expr e2)
  | `Return es -> Printf.sprintf "Return %s" (string_of_exprs es)
  | `Label l -> "Label " ^ l
  | `CJump (e, t, f) ->
      Printf.sprintf "CJump(%s, %s, %s)" (string_of_expr e) t f
  | `Jump e -> Printf.sprintf "Jump %s" (string_of_expr e)

open Temp

let def ?(init = Virtual.Set.empty) = function
  | `Move ((`Temp _ as t), _) -> Set.add init t
  | `Call (m, _, _) ->
      let range = List.range ~start:`inclusive ~stop:`inclusive 1 m in
      List.fold range ~init ~f:(fun acc i -> Set.add acc (`Rv i))
  | #stmt -> init

let rec use_expr ~init : expr -> Virtual.Set.t = function
  | `Name _ | `Const _ -> init
  | `Bop (_, e1, e2) -> use_expr2 ~init e1 e2
  | #Virtual.t as t -> Set.add init t
  | `Mem e -> use_expr ~init e

and use_expr2 ~init e1 e2 =
  let s = use_expr ~init e1 in
  use_expr ~init:s e2

let use_exprs ~init = List.fold ~init ~f:(fun acc -> use_expr ~init:acc)

let use_stmt ~init : stmt -> Virtual.Set.t = function
  | `Call (_, e, es) -> use_exprs ~init:(use_expr ~init e) es
  | `Move (e1, e2) -> use_expr2 ~init (e1 :> expr) e2
  | `Return es -> use_exprs ~init es
  | `Label _ -> init
  | `CJump (e, _, _) | `Jump e -> use_expr ~init e

let use ?(init = Virtual.Set.empty) = use_stmt ~init

let rec map_expr ~f = function
  | #Temp.Virtual.t as t -> (f t :> expr)
  | `Bop (op, e1, e2) -> `Bop (op, map_expr ~f e1, map_expr ~f e2)
  | #expr as e -> e

let map_exprs ~f = List.map ~f:(map_expr ~f)

let map_stmt ~f = function
  | `Call (n, e, es) -> `Call (n, map_expr ~f e, map_exprs ~f es)
  | `CJump (e, l1, l2) -> `CJump (map_expr ~f e, l1, l2)
  | `Jump e -> `Jump (map_expr ~f e)
  | `Return es -> `Return (map_exprs ~f es)
  | `Move ((`Temp _ as t), e) -> `Move (t, map_expr ~f e)
  | #stmt as s -> s

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
  | #stmt -> add_fallthrough src vs

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

(** [corece e] is [e :> expr] *)
let coerce e = (e :> expr)

(** [rev_lower_expr ~init:\[sm; ...; s1\] s] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions equivalent to IR statement [s] *)
let rec rev_lower_expr ~gensym ~init : Mir.expr -> stmt list * expr =
  function
  | (`Const _ | `Name _) as e -> (init, e)
  | `Call (i, e, es) -> rev_lower_call ~gensym ~init i e es
  | `ESeq (s, e) -> rev_lower_eseq ~gensym ~init s e
  | `Bop (op, e1, e2) -> rev_lower_bop ~gensym ~init op e1 e2
  | `Mem e -> (rev_lower_mem ~gensym ~init e :> stmt list * expr)
  | #Temp.Virtual.t as e -> (init, e)

(** [rev_lower_dest ~init:\[sm; ...; s1\] s] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions equivalent to IR statement [s] *)
and rev_lower_dest ~gensym ~init = function
  | `Temp _ as e -> (init, e)
  | `Mem e -> rev_lower_mem ~gensym ~init e

(** [rev_lower_call ~init:\[sm; ...; s1\] e es] is
    ([sn; ...; sm-1; sm; ... s1], e') if [sm-1; ...; sn] are the
    sequence of lowered IR expressions having the same side effects as
    IR statement [`Call (e, es)] and [e'] is the pure, lowered IR
    expression equivalent to the result computed by [`Call (e, es)] *)
and rev_lower_call ~gensym ~init i e es =
  let t = IrGensym.Temp.fresh gensym in
  let move = `Move (t, `Rv 1) in
  (move :: rev_lower_call_stmt ~gensym ~init i e es, t)

(** [rev_lower_eseq ~init:\[sm; ...; s1\] s e] is
    ([sn; ...; sm-1; sm; ... s1], e') if [sm-1; ...; sn] are the
    sequence of lowered IR expressions having the same side effects as
    IR statement [`ESeq (e, s)] and [e'] is the pure, lowered IR
    expression equivalent to the result computed by [`ESeq (e, s)] *)
and rev_lower_eseq ~gensym ~init s e =
  let s' = rev_lower_stmt ~gensym ~init s in
  rev_lower_expr ~gensym ~init:s' e

(** [expr2_general ~gensym ~init e1 e2] is a triple [(s, e1', e2')]
    where [s] is a sequence of statements having the effects of [init],
    [e1], and [e2] in reverse, [e1'] is the pure expression computing
    the same value of [e1], and [e2'] is the pure expression computing
    the same value as [e2], assuming [e1] and [e2] do not commute *)
and expr2_general ~gensym ~init e1 e2 : stmt list * dest * expr =
  let t = IrGensym.Temp.fresh gensym in
  let s1, e1' = rev_lower_expr ~gensym ~init e1 in
  let init = `Move (t, e1') :: s1 in
  let s2, e2' = rev_lower_expr ~gensym ~init e2 in
  (s2, t, e2')

(** [expr2_commute ~gensym ~init e1 e2] is a triple [(s, e1', e2')]
    where [s] is a sequence of statements having the effects of [init],
    [e1], and [e2] in reverse, [e1'] is the pure expression computing
    the same value of [e1], and [e2'] is the pure expression computing
    the same value as [e2], assuming [e1] and [e2] commute. *)
and expr2_commute ~gensym ~init e1 e2 =
  let s1, e1 = rev_lower_expr ~gensym ~init e1 in
  let s2, e2 = rev_lower_expr ~gensym ~init:s1 e2 in
  (s2, e1, e2)

(** [rev_lower_expr2 ~gensym ~init e1 e2] is a triple [(s, e1', e2')]
    where [s] is a sequence of statements having the effects of [init],
    [e1], and [e2] in reverse, [e1'] is the pure expression computing
    the same value of [e1], and [e2'] is the pure expression computing
    the same value as [e2]. *)
and rev_lower_expr2 ~gensym ~init e1 e2 =
  if Mir.commute e1 e2 then expr2_commute ~gensym ~init e1 e2
  else Tuple3.map_snd (expr2_general ~gensym ~init e1 e2) ~f:coerce

(** [rev_lower_bop ~init:\[sm; ...; s1\] op e1 e2] is
    ([sn; ...; sm-1; sm; ... s1], e') if [sm-1; ...; sn] are the
    sequence of lowered IR expressions having the same side effects as
    IR statement [`Bop (op, e1, e2)] and [e'] is the pure, lowered IR
    expression equivalent to the result computed by [`Bop (op, e1, e2)] *)
and rev_lower_bop ~gensym ~init op e1 e2 =
  let s, e1, e2 = rev_lower_expr2 ~gensym ~init e1 e2 in
  (s, `Bop (op, e1, e2))

(** [rev_lower_mem ~init:\[sm; ...; s1\] e] is
    ([sn; ...; sm-1; sm; ... s1], e') if [sm-1; ...; sn] are the
    sequence of lowered IR expressions having the same side effects as
    IR expresion [e] and [e'] is the pure, lowered IR expression
    equivalent to the result computed by [`Mem e] *)
and rev_lower_mem ~gensym ~init e =
  e
  |> rev_lower_expr ~gensym ~init
  |> Tuple2.map_snd ~f:(fun e -> `Mem e)

(** [rev_lower_stmt ~init:\[sm; ...; s1\] s] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [s] *)
and rev_lower_stmt ~gensym ~init : Mir.stmt -> stmt list = function
  | `Label l as s -> s :: init
  | `Move (dest, e) -> rev_lower_move ~gensym ~init dest e
  | `Jump e -> rev_lower_jump ~gensym ~init e
  | `Return es -> rev_lower_return ~gensym ~init es
  | `Call (i, e, es) -> rev_lower_call_stmt ~gensym ~init i e es
  | `CJump (e, l1, l2) -> rev_lower_cjump ~gensym ~init e l1 l2
  | `Seq sv -> rev_lower_seq ~gensym ~init sv

(** [dest_expr_commute ~gensym ~init dst src] is a triple
    [(s, dst', src')] where [s] is a sequence of statements having the
    effects of [init], [dst], and [src] in reverse, [dst'] is the pure
    expression computing the same value of [dst], and [src'] is the pure
    expression computing the same value as [src], assuming [dst] and
    [src] commute *)
and dest_expr_commute ~gensym ~init dst src =
  let s, dst = rev_lower_dest ~gensym ~init dst in
  let s, src = rev_lower_expr ~gensym ~init:s src in
  (s, dst, src)

(** [rev_lower_dest_expr ~gensym ~init dst src] is a triple
    [(s, dst', src')] where [s] is a sequence of statements having the
    effects of [init], [dst], and [src] in reverse, [dst'] is the pure
    expression computing the same value of [dst], and [src'] is the pure
    expression computing the same value as [src] *)
and rev_lower_dest_expr ~gensym ~init dst src =
  let commute () = dest_expr_commute ~gensym ~init dst src in
  match dst with
  | `Temp _ -> commute ()
  | `Mem _ when Mir.commute (dst :> Mir.expr) src -> commute ()
  | `Mem e ->
      let f = Fn.compose ( ! ) coerce in
      Tuple3.map_snd ~f (expr2_general ~gensym ~init e src)

(** [rev_lower_move ~init:\[sm; ...; s1\] dst e] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [`Move (dst, e)] *)
and rev_lower_move ~gensym ~init dst src =
  let s, dst, src = rev_lower_dest_expr ~gensym ~init dst src in
  `Move (dst, src) :: s

(** [rev_move_temp ~init:\[sm; ...; s1\] dst src] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [`Move (t, src)], where [t] is a temporary *)
and rev_lower_move_temp ~gensym ~init t src =
  let s, e = rev_lower_expr ~gensym ~init src in
  `Move (t, e) :: s

(** [rev_lower_move ~init:\[sm; ...; s1\] e] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [`Jump e] *)
and rev_lower_jump ~gensym ~init e =
  let s, e' = rev_lower_expr ~gensym ~init e in
  `Jump e' :: s

(** [rev_lower_moves ~init:\[sm; ...; s1\] \[e1; ...; ei\]] is
    ([t1; ...; ti], [sn; ...; sm-1; sm; ... s1] if each [tj] is a fresh
    temp and [sm-1; ...; sn] are the sequence of lowered IR expressions
    having the same side effects as IR statements
    [T\[`Move (t1, e1)\]; ...; T\[`Move (ti, ei)\]], where [T\[.\]] is
    the lowering function *)
and rev_lower_moves ~gensym ~init es =
  let f (ts, init) e =
    let t = IrGensym.Temp.fresh gensym in
    (t :: ts, rev_lower_move_temp ~gensym ~init t e)
  in
  es |> List.fold ~f ~init:([], init) |> Tuple2.map_fst ~f:List.rev

(** [rev_lower_return ~init:\[sm; ...; s1\] \[e1; ...; ei\]] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [`Return \[e1; ...; ei\]] *)
and rev_lower_return ~gensym ~init es =
  let ts, seq = rev_lower_moves ~gensym ~init es in
  `Return ts :: seq

(** [rev_lower_move ~init:\[sm; ...; s1\] e l1 l2] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [`CJump (e, l1, l2)] *)
and rev_lower_cjump ~gensym ~init e l1 l2 =
  let s, e' = rev_lower_expr ~gensym ~init e in
  `CJump (e', l1, l2) :: s

(** [rev_lower_call_stmt ~init:\[sm; ...; s1\] e0 \[e1; ...; ei\]] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [Call(e0, e1,..., ei)] *)
and rev_lower_call_stmt ~gensym ~init i e es =
  let seq, e' = rev_lower_expr ~gensym ~init e in
  let ts, seq = rev_lower_moves ~gensym ~init:seq es in
  `Call (i, e', ts) :: seq

(** [rev_lower_seq ~init:\[sm; ...; s1\] seq] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [`Seq seq] *)
and rev_lower_seq ~gensym ~init seq =
  List.fold ~f:(fun acc -> rev_lower_stmt ~gensym ~init:acc) ~init seq

(** [lower_seq seq] is the sequence of lowered ir statements having the
    same effect as [seq] *)
let lower_seq ~gensym =
  Fn.compose List.rev (rev_lower_seq ~gensym ~init:[])

let lower_function ~(opt : Opt.t) ~gensym l b a r =
  `Func (l, lower_seq ~gensym b, a, r)

let lower_toplevel ~opt ~gensym = function
  | `Data _ as d -> d
  | `Func (l, b, a, r) -> lower_function ~opt ~gensym l b a r

let lower ~opt ~gensym = List.map ~f:(lower_toplevel ~opt ~gensym)
