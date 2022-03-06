open Sexplib.Std

type tau = Tau.t [@@deriving sexp_of]

type expr =
  [ tau
  | `Tuple of tau list
  ]
[@@deriving sexp_of]

type term =
  [ expr
  | `Unit
  ]
[@@deriving sexp_of]

type stmt =
  [ `Unit
  | `Void
  ]
[@@deriving sexp_of]

type id =
  | Var of tau
  | Fn of term * term
[@@deriving sexp_of]

let lub t1 t2 = match (t1, t2) with `Void, `Void -> `Void | _ -> `Unit
