open Subtype

type expr = expr Subtype.expr

type stmt =
  [ expr Subtype.stmt
  | `Call of expr * expr list
  | `CJump of expr * label
  | `Mem of 'expr * 'expr
  | `Bop of binop * 'expr * 'expr
  ]

(* need to reverse the list *)
let rec lower_expr expr =
  match expr with
  | `Const _  | `Name _ | `Temp _ -> ([], e)
  | `Call e, elist -> let (sv1, e1) = lower_expr e in
    let e1temp = make_fresh_temp () in
    let translated_e = sv1 @ [`Move (`Temp e1temp, e1)] in
    let translated_calls = List.fold_right elist (fun el acc ->
        let (sv, e') = lower_expr el in
        List.concat [acc; sv; [`Move (`Temp (make_fresh_temp ()), e')]]) [] in
    let new_temp = `Temp (make_fresh_temp ()) in
    (List.concat [translated_e; translated_calls; [`Call (e1temp, temps); `Move (new_temp, `Temp "_RV1")]], new_temp)
  | `ESeq s, e -> let sv = lower_stmt s in let (sv', e') = lower_expr e in
    (sv @ sv', e')
  | `Bop op, e1,  e2 -> let (sv1, e1') = lower_expr e1 in
    let (sv2, e2') = lower_expr e2 in
    let tmp = `Temp (make_fresh_temp ()) in
    (List.concat [sv1; [`IRMove (tmp, e1')]; sv2], `Bop (op, tmp, e2'))
  | `Mem e -> let (sv, e') = lower_expr e in (sv, `Mem e')

and lower_stmt stmt =
  match stmt with
  | `Move dest, e -> let (sv2', e2') = lower_expr e in
    begin
      match dest with
      | `Temp _ -> sv2' @ [`Move(dest, e2')] (* commute rule *)
      | `Mem e1 -> let (sv1', e1') = lower_expr e1 in (* general rule *)
        let new_temp = `Temp (make_fresh_temp ()) in
        List.concat[sv1'; [`Move(new_temp, e1')]; sv2'; [`Move (`Mem (new_temp), e2')]];
    end
  | `Jump e -> let (sv, e') = lower_expr e in sv @ [`Jump e']
  | `Label l -> [stmt]
  | `Return elist -> let (sv, ev) = List.fold_right list
                         (fun (sl, el) expr -> let (s, e) = lower_expr expr in
                           (sl @ s, e1 @ e)) ([], []) in sv @ ev
  | `CJump e, l1, l2 -> let (sv, e') = lower_expr e in sv @ [`CJump (e', l1, l2)]
  | `Seq slist -> List.map lower_stmt slist |> List.concat
