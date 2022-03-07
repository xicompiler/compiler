open! Core

type t =
  [ FnType.t
  | `Var of Tau.t
  ]
[@@deriving sexp_of]

let var t = `Var t
let fn_decl ?arg ?ret () = (FnType.decl ?arg ?ret () :> t)
let fn_defn ?arg ?ret () = (FnType.defn ?arg ?ret () :> t)
