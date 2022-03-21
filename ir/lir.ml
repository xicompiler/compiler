open Subtype

type expr = expr Subtype.expr

type stmt =
  [ expr Subtype.stmt
  | `Call of expr * expr list
  | `CJump of expr * label
  ]

let make_fresh_temp () = failwith "unimplemented"

(* need to reverse the list *)
let rec lower_expr expr =
  match expr with
  | (`Const _ | `Name _ | `Temp _) as e -> ([], e)
  | `Call (e, es) -> lower_call e es
  | `ESeq (s, e) -> lower_eseq s e
  | `Bop (op, e1, e2) -> lower_bop op e1 e2
  | `Mem e -> lower_mem e

and lower_call e es =
  let sv1, e1 = lower_expr e in
  let t = make_fresh_temp () in
  let translated_e = sv1 @ [ `Move (`Temp t, e1) ] in
  let translated_calls =
    List.fold_right es
      (fun el acc ->
        let sv, e' = lower_expr el in
        List.concat
          [ acc; sv; [ `Move (`Temp (make_fresh_temp ()), e') ] ])
      []
  in
  let new_temp = `Temp (make_fresh_temp ()) in
  ( List.concat
      [
        translated_e;
        translated_calls;
        [ `Call (e1temp, temps); `Move (new_temp, `Temp "_RV1") ];
      ],
    new_temp )

and lower_eseq s e =
  let sv = lower_stmt s in
  let sv', e' = lower_expr e in
  (sv @ sv', e')

and lower_bop op e1 e2 =
  let sv1, e1' = lower_expr e1 in
  let sv2, e2' = lower_expr e2 in
  let t = `Temp (make_fresh_temp ()) in
  (List.concat [ sv1; [ `IRMove (t, e1') ]; sv2 ], `Bop (op, t, e2'))

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
  let sv2', e2' = lower_expr e in
  match dest with
  | `Temp _ -> sv2' @ [ `Move (dest, e2') ] (* commute rule *)
  | `Mem e1 ->
      let sv1', e1' = lower_expr e1 in
      (* general rule *)
      let new_temp = `Temp (make_fresh_temp ()) in
      List.concat
        [
          sv1';
          [ `Move (new_temp, e1') ];
          sv2';
          [ `Move (`Mem new_temp, e2') ];
        ]

and lower_jump e =
  let sv, e' = lower_expr e in
  sv @ [ `Jump e' ]

and lower_return es =
  let sv, ev =
    List.fold_right list
      (fun (sl, el) expr ->
        let s, e = lower_expr expr in
        (sl @ s, e1 @ e))
      ([], [])
  in
  sv @ ev

and lower_cjump e l1 l2 =
  let sv, e' = lower_expr e in
  sv @ [ `CJump (e', l1, l2) ]

and lower_seq sv = List.map lower_stmt sv |> List.concat
