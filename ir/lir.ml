open Core
open Subtype

type expr = expr Subtype.expr
type stmt = expr Subtype.cjump2

type toplevel =
  [ `Func of Subtype.label * stmt list
  | `Data of Subtype.label * Int64.t
  ]

let log_neg = Subtype.log_neg

type t = toplevel list

(** [fresh_temp ()] is the symbol generator for temps *)
let fresh_temp = Temp.generator ()

(** [rv1] is the virtual egister storing the first return value *)
let rv1 = `Temp "_RV1"

(** [rev_lower_expr ~init:\[sm; ...; s1\] s] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions equivalent to IR statement [s] *)
let rec rev_lower_expr ~init : Mir.expr -> stmt list * expr = function
  | (`Const _ | `Name _ | `Temp _) as e -> (init, e)
  | `Call (i, e, es) -> rev_lower_call ~init i e es
  | `ESeq (s, e) -> rev_lower_eseq ~init s e
  | `Bop (op, e1, e2) -> rev_lower_bop ~init op e1 e2
  | `Mem e -> rev_lower_mem ~init e

(** [rev_lower_call ~init:\[sm; ...; s1\] e es] is
    ([sn; ...; sm-1; sm; ... s1], e') if [sm-1; ...; sn] are the
    sequence of lowered IR expressions having the same side effects as
    IR statement [`Call (e, es)] and [e'] is the pure, lowered IR
    expression equivalent to the result computed by [`Call (e, es)] *)
and rev_lower_call ~init i e es =
  let t = fresh_temp () in
  (`Move (t, rv1) :: rev_lower_call_stmt ~init i e es, t)

(** [rev_lower_eseq ~init:\[sm; ...; s1\] s e] is
    ([sn; ...; sm-1; sm; ... s1], e') if [sm-1; ...; sn] are the
    sequence of lowered IR expressions having the same side effects as
    IR statement [`ESeq (e, s)] and [e'] is the pure, lowered IR
    expression equivalent to the result computed by [`ESeq (e, s)] *)
and rev_lower_eseq ~init s e =
  let s' = rev_lower_stmt ~init s in
  rev_lower_expr ~init:s' e

(** [rev_lower_bop ~init:\[sm; ...; s1\] op e1 e2] is
    ([sn; ...; sm-1; sm; ... s1], e') if [sm-1; ...; sn] are the
    sequence of lowered IR expressions having the same side effects as
    IR statement [`Bop (op, e1, e2)] and [e'] is the pure, lowered IR
    expression equivalent to the result computed by [`Bop (op, e1, e2)] *)
and rev_lower_bop ~init op e1 e2 =
  let t = fresh_temp () in
  let s1, e1' = rev_lower_expr ~init e1 in
  let init = `Move (t, e1') :: s1 in
  let s2, e2' = rev_lower_expr ~init e2 in
  (s2, `Bop (op, t, e2'))

(** [rev_lower_mem ~init:\[sm; ...; s1\] e] is
    ([sn; ...; sm-1; sm; ... s1], e') if [sm-1; ...; sn] are the
    sequence of lowered IR expressions having the same side effects as
    IR expresion [e] and [e'] is the pure, lowered IR expression
    equivalent to the result computed by [`Mem e] *)
and rev_lower_mem ~init e =
  e |> rev_lower_expr ~init |> Tuple2.map_snd ~f:(fun e -> `Mem e)

(** [rev_lower_stmt ~init:\[sm; ...; s1\] s] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [s] *)
and rev_lower_stmt ~init : Mir.stmt -> stmt list = function
  | `Label l as s -> s :: init
  | `Move (dest, e) -> rev_lower_move ~init dest e
  | `Jump e -> rev_lower_jump ~init e
  | `Return es -> rev_lower_return ~init es
  | `Call (i, e, es) -> rev_lower_call_stmt ~init i e es
  | `CJump (e, l1, l2) -> rev_lower_cjump ~init e l1 l2
  | `Seq sv -> rev_lower_seq ~init sv

(** [rev_lower_move ~init:\[sm; ...; s1\] dst e] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [`Move (dst, e)] *)
and rev_lower_move ~init dst src =
  match dst with
  | `Temp _ as t -> rev_lower_move_temp ~init t src
  | `Mem addr -> rev_lower_move_mem ~init addr src

(** [rev_move_temp ~init:\[sm; ...; s1\] dst src] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [`Move (t, src)], where [t] is a temporary *)
and rev_lower_move_temp ~init t src =
  let s, e = rev_lower_expr ~init src in
  `Move (t, e) :: s

(** [rev_lower_move_mem ~init:\[sm; ...; s1\] addr src] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [`Move (`Mem addr, src)] *)
and rev_lower_move_mem ~init addr src =
  let s_addr, addr' = rev_lower_expr ~init addr in
  let t = fresh_temp () in
  let init = `Move (t, addr') :: s_addr in
  let s_src, src' = rev_lower_expr ~init src in
  `Move (`Mem t, src') :: s_src

(** [rev_lower_move ~init:\[sm; ...; s1\] e] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [`Jump e] *)
and rev_lower_jump ~init e =
  let s, e' = rev_lower_expr ~init e in
  `Jump e' :: s

(** [rev_lower_moves ~init:\[sm; ...; s1\] \[e1; ...; ei\]] is
    ([t1; ...; ti], [sn; ...; sm-1; sm; ... s1] if each [tj] is a fresh
    temp and [sm-1; ...; sn] are the sequence of lowered IR expressions
    having the same side effects as IR statements
    [T\[`Move (t1, e1)\]; ...; T\[`Move (ti, ei)\]], where [T\[.\]] is
    the lowering function *)
and rev_lower_moves ~init es =
  let f (ts, init) e =
    let t = fresh_temp () in
    (t :: ts, rev_lower_move_temp ~init t e)
  in
  es |> List.fold ~f ~init:([], init) |> Tuple2.map_fst ~f:List.rev

(** [rev_lower_return ~init:\[sm; ...; s1\] \[e1; ...; ei\]] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [`Return \[e1; ...; ei\]] *)
and rev_lower_return ~init es =
  let ts, seq = rev_lower_moves ~init es in
  `Return ts :: seq

(** [rev_lower_move ~init:\[sm; ...; s1\] e l1 l2] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [`CJump (e, l1, l2)] *)
and rev_lower_cjump ~init e l1 l2 =
  let s, e' = rev_lower_expr ~init e in
  `CJump (e', l1, l2) :: s

(** [rev_lower_call_stmt ~init:\[sm; ...; s1\] e0 \[e1; ...; ei\]] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [Call(e0, e1,..., ei)] *)
and rev_lower_call_stmt ~init i e es =
  let ts, seq = rev_lower_moves ~init (e :: es) in
  let t0, ts = Util.List.hd_tl_exn ts in
  `Call (i, t0, ts) :: seq

(** [rev_lower_seq ~init:\[sm; ...; s1\] seq] is
    [sn; ...; sm-1; sm; ... s1] if [sm-1; ...; sn] are the sequence of
    lowered IR expressions having the same side effects as IR statement
    [`Seq seq] *)
and rev_lower_seq ~init seq =
  List.fold ~f:(fun acc -> rev_lower_stmt ~init:acc) ~init seq

(** [lower_seq seq] is the sequence of lowered ir statements having the
    same effect as [seq] *)
let lower_seq = Fn.compose List.rev (rev_lower_seq ~init:[])

let lower_func l b = `Func (l, lower_seq b)

let lower_toplevel = function
  | `Data _ as d -> d
  | `Func (l, b) -> lower_func l b

let lower = List.map ~f:lower_toplevel
