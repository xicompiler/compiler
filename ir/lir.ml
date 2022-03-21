open Core
open Subtype

type expr = expr Subtype.expr

type stmt =
  [ expr Subtype.stmt
  | `Call of expr * expr list
  | `CJump of expr * label
  ]

let make_fresh_temp () = failwith "unimplemented"

let rec lower_expr expr =
  match expr with
  | (`Const _ | `Name _ | `Temp _) as e -> ([], e)
  | `Call (e, es) -> lower_call e es
  | `ESeq (s, e) -> lower_eseq s e
  | `Bop (op, e1, e2) -> lower_bop op e1 e2
  | `Mem e -> lower_mem e

and lower_call e es =
  let sv0, e0 = lower_expr e in
  let t0 = make_fresh_temp () in
  let f (ts, acc) el =
    let sv, e' = lower_expr el in
    let t = make_fresh_temp () in
    (t :: ts, List.concat [ acc; sv; [ `Move (`Temp t, e') ] ])
  in
  let ts, args = List.fold ~init:([], []) es ~f in
  let tr = make_fresh_temp () in
  ( List.concat
      [
        sv0 @ [ `Move (`Temp t0, e0) ];
        args;
        [ `Call (t0, List.rev ts); `Move (`Temp tr, `Temp "_RV1") ];
      ],
    tr )

and lower_eseq s e =
  let sv = lower_stmt s in
  let sv', e' = lower_expr e in
  (sv @ sv', e')

and lower_bop op e1 e2 =
  let sv1, e1' = lower_expr e1 in
  let sv2, e2' = lower_expr e2 in
  let t = make_fresh_temp () in
  ( List.concat [ sv1; [ `Move (`Temp t, e1') ]; sv2 ],
    `Bop (op, `Temp t, e2') )

and lower_mem e =
  let sv, e' = lower_expr e in
  (sv, `Mem e')

and lower_stmt stmt =
  match stmt with
  | `Label l as s -> [ s ]
  | `Move (dest, e) -> lower_move dest e
  | `Jump e -> lower_jump e
  | `Return es -> lower_return es
  | `CJump (e, l1, l2) -> lower_cjump e l1 l2
  | `Seq sv -> lower_seq sv

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
      let t = make_fresh_temp () in
      List.concat
        [
          sv1;
          [ `Move (`Temp t, e1') ];
          sv2;
          [ `Move (`Mem (`Temp t), e2) ];
        ]

and lower_jump e =
  let sv, e' = lower_expr e in
  sv @ [ `Jump e' ]

and lower_return es =
  let f (ts_rev, moves_rev) el =
    let t = make_fresh_temp () in
    let sv, e = lower_expr el in
    let move = lower_move t e in
    (t :: ts_rev, move :: moves_rev)
  in
  let ts_rev, moves_rev = List.fold es ~f ~init:([], []) in
  let ts = List.rev ts_rev in
  let moves = moves_rev |> List.rev |> List.concat in
  List.concat [ moves; [ `Return ts ] ]

and lower_cjump e l1 l2 =
  let sv, e' = lower_expr e in
  sv @ [ `CJump (e', l1, l2) ]

and lower_seq sv = sv |> List.map ~f:lower_stmt |> List.concat
