type tau = Tau.t

type expr =
  [ tau
  | `Tuple of tau list
  ]

type term =
  [ expr
  | `Unit
  ]

type stmt =
  [ `Unit
  | `Void
  ]

type id =
  | Var of tau
  | Fn of term * term

let lub t1 t2 =
  match (t1, t2) with
  | `Void, `Void -> `Void
  | _ -> `Unit
