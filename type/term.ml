open Core

type t =
  [ Expr.t
  | `Unit
  ]
[@@deriving sexp_of]

let to_tau_list = function
  | `Unit -> []
  | `Tuple ts -> ts
  | #Tau.t as t -> [ t ]

let of_tau_list = function
  | [] -> `Unit
  | [ typ ] -> (typ :> t)
  | lst -> `Tuple lst

let to_expr = function `Unit -> `Tuple [] | #Expr.t as typ -> typ

let equal t1 t2 =
  match (t1, t2) with
  | `Unit, `Unit -> true
  | (#Expr.t as t1), (#Expr.t as t2) -> Expr.equal t1 t2
  | `Unit, _ | _, `Unit -> false

let or_unit = function Some typ -> (typ :> t) | None -> `Unit
