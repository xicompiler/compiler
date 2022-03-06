open! Core

type t =
  [ Expr.t
  | `Unit
  ]
[@@deriving sexp_of]

let equal t1 t2 =
  match (t1, t2) with
  | `Unit, `Unit -> true
  | (#Expr.t as t1), (#Expr.t as t2) -> Expr.equal t1 t2
  | `Unit, _ | _, `Unit -> false
