open Subtype

type expr = expr Subtype.expr

type stmt =
  [ expr Subtype.stmt
  | `Call of expr * expr list
  | `Seq of stmt list
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
                      let translated_calls = List.map elist ~f:(fun el ->
                          let (sv, e') = lower_expr el in
                          sv @ [`Move (`Temp (make_fresh_temp ()), e')]) in
                      (translated_e @ translated_calls @ [`Call (e1temp, temps)], `Temp "_RV1")
  | `ESeq s, e -> let sv = lower_stmt s in let (sv', e') = lower_expr e in
  (sv @ sv', e')
  | `Bop op, e1,  e2 -> failwith "unimplmented"
  | `Mem e -> let (sv, e') = lower_expr e in (sv, `Mem e')

and lower_stmt stmt =
  match stmt with
  | `Move e1, e2 -> failwith "unimplemented"
  | `Jump e -> let (sv, e') = lower_expr e in sv @ [`Jump e']
  | `Label l -> [stmt]
  | `Return elist -> failwith "unimplemented"
  | `CJump e, l1, l2 -> let (sv, e') = lower_expr e in sv @ [`CJump (e', l1, l2)]
