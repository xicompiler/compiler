open Core
open Subtype

type expr = expr Subtype.expr
type stmt = expr Subtype.cjump2

let log_neg = Subtype.log_neg

type t = stmt list

(** [fresh_temp ()] is the symbol generator for temps *)
let fresh_temp = Temp.generator ()

(** [lower_expr expr] is the lowered form of mir expression [expr] *)
let rec lower_expr expr =
  match expr with
  | (`Const _ | `Name _ | `Temp _) as e -> ([], e)
  | `Call (e, es) -> lower_call e es
  | `ESeq (s, e) -> lower_eseq s e
  | `Bop (op, e1, e2) -> lower_bop op e1 e2
  | `Mem e -> lower_mem e

(** [lower_call e es] is the lowered form of a call expression with
    function name [e] and arguments [es] *)
and lower_call e es =
  let sv0, e0 = lower_expr e in
  let t0 = fresh_temp () in
  let f (ts, acc) el =
    let sv, e' = lower_expr el in
    let t = fresh_temp () in
    (t :: ts, List.concat [ acc; sv; [ `Move (t, e') ] ])
  in
  let ts, args = List.fold ~init:([], []) es ~f in
  let tr = fresh_temp () in
  ( List.concat
      [
        sv0 @ [ `Move (t0, e0) ];
        args;
        [ `Call (t0, List.rev ts); `Move (tr, `Temp "_RV1") ];
      ],
    tr )

(** [lower_eseq s e] is the lowered form of an expression sequence with
    statement [s] and expression [e] *)
and lower_eseq s e =
  let sv = lower_stmt s in
  let sv', e' = lower_expr e in
  (sv @ sv', e')

(** [lower_bop op e1 e2] is the lowered form of operator expression
    [op e1 e2] *)
and lower_bop op e1 e2 =
  let sv1, e1' = lower_expr e1 in
  let sv2, e2' = lower_expr e2 in
  let t = fresh_temp () in
  (List.concat [ sv1; [ `Move (t, e1') ]; sv2 ], `Bop (op, t, e2'))

(** [lower_mem e] is the lowered form of a mem expression of [e] *)
and lower_mem e =
  let sv, e' = lower_expr e in
  (sv, `Mem e')

(** [lower_stmt stmt] is the lowered form of mir statement [stmt] *)
and lower_stmt stmt =
  match stmt with
  | `Label l as s -> [ s ]
  | `Move (dest, e) -> lower_move dest e
  | `Jump e -> lower_jump e
  | `Return es -> lower_return es
  | `CJump (e, l1, l2) -> lower_cjump e l1 l2
  | `Seq sv -> lower_seq sv

(** [lower_move dest e] is the lowered form of a move statement with
    destination [dest] and mir expression [e] *)
and lower_move dest e =
  let sv2, e2 = lower_expr e in
  match dest with
  | `Temp _ as t ->
      (* commute rule *)
      let sv1, dest' = lower_expr t in
      List.concat [ sv1; sv2; [ `Move (dest', e2) ] ]
  | `Mem e1 ->
      (* general rule *)
      let sv1, e1' = lower_expr e1 in
      let t = fresh_temp () in
      List.concat
        [ sv1; [ `Move (t, e1') ]; sv2; [ `Move (`Mem t, e2) ] ]

(** [lower_jump e] is the lowered form of a jump statement to [e] *)
and lower_jump e =
  let sv, e' = lower_expr e in
  sv @ [ `Jump e' ]

(** [lower_return es] is the lowered form of a return statement with
    expressions [es] *)
and lower_return es =
  let f (ts_rev, moves_rev) el =
    let t = fresh_temp () in
    let sv, e = lower_expr el in
    let move = lower_move t e in
    (t :: ts_rev, move :: moves_rev)
  in
  let ts_rev, moves_rev = List.fold es ~f ~init:([], []) in
  let ts = List.rev ts_rev in
  let moves = moves_rev |> List.rev |> List.concat in
  List.concat [ moves; [ `Return ts ] ]

(** [lower_cjump e l1 l2] is the lowered form of a conditional jump with
    condition [e] and labels [l1] and [l2] *)
and lower_cjump e l1 l2 =
  let sv, e' = lower_expr e in
  sv @ [ `CJump (e', l1, l2) ]

(** [lower_seq sv] is the lowered form of sequence [seq] *)
and lower_seq sv = sv |> List.map ~f:lower_stmt |> List.concat
