open Core
open Option.Let_syntax
module Mir = Mir
module Lir = Lir
module Reorder = Reorder

let translate ast =
  let mir = Mir.translate ast in
  let lir = Lir.lower mir in
  (* TODO fix gensym *)
  let gensym = Subtype.Label.generator () in
  Reorder.reorder lir ~gensym

(** [const_of_base b] is [`Const r] if [b] is [Some r] and [None]
    otherwise *)
let const_of_base b =
  let%map r = b in
  `Const r

(** [const_fold_bop_opt bop e1 e2] is the IR node [e1 bop e2] where all
    constant expressions have been folded *)
let const_fold_bop_opt bop e1 e2 =
  match (e1, e2) with
  | `Const i1, `Const i2 -> const_of_base (Op.eval bop i1 i2)
  | _ -> None

(** [const_fold_expr expr] is LIR expression [expr] constant folded *)
let rec const_fold_expr : Lir.expr -> Lir.expr = function
  | (`Const _ | `Name _) as e -> e
  | `Bop (op, e1, e2) -> const_fold_bop op e1 e2
  | #Subtype.dest as dest -> (const_fold_dest dest :> Lir.expr)

(** [const_fold_dest] is [dest] constant folded *)
and const_fold_dest : Lir.expr Subtype.dest -> Lir.expr Subtype.dest =
  function
  | `Mem e -> `Mem (const_fold_expr e)
  | `Temp _ as t -> t

(** [const_fold_bop op e1 e2] is operation [e1 op e2] constant folded *)
and const_fold_bop op e1 e2 =
  let e1 = const_fold_expr e1 in
  let e2 = const_fold_expr e2 in
  let default () = `Bop (op, e1, e2) in
  Util.Option.Lazy.value ~default (const_fold_bop_opt op e1 e2)

(** [const_fold_stmt stmt] is LIR statement [stmt] constant folded *)
let const_fold_stmt : Reorder.stmt -> Reorder.stmt = function
  | `Label l as s -> s
  | `CJump (e, l) -> `CJump (const_fold_expr e, l)
  | `Call (e, es) ->
      let e = const_fold_expr e in
      let es = List.map es ~f:const_fold_expr in
      `Call (e, es)
  | `Jump e -> `Jump (const_fold_expr e)
  | `Move (dest, e) ->
      let dest = const_fold_dest dest in
      let e = const_fold_expr e in
      `Move (dest, e)
  | `Return es -> `Return (List.map es ~f:const_fold_expr)

let const_fold = List.map ~f:const_fold_stmt
