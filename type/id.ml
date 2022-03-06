open! Core

type t =
  | Var of Tau.t
  | Fn of Term.t * Term.t
[@@deriving sexp_of]

let var t = Var t
let fn ~arg ~ret = Fn ((arg :> Term.t), (ret :> Term.t))
let proc arg = fn ~arg ~ret:`Unit
let fn_unit ret = fn ~arg:`Unit ~ret
