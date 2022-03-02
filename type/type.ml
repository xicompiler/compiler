open Core

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

type error =
  | Unbound
  | IdMismatch of string
  | ExpectedTau
  | ExpectedArray
  | ExpectedFun
  | ExpectedTerm
  | ArgMismatch
  | Mismatch of expr * expr

type nonrec 'a result = ('a, error) result
type context = id Context.t

module Context = struct
  include Context

  module Fn = struct
    type t = {
      context : context;
      ret : term;
    }

    let context { context } = context
    let ret { ret } = ret
  end
end

module Tau = Tau

let tau_of_expr = function
  | (`Int | `Bool | `Array _) as t -> Some t
  | `Tuple _ -> None

let tau_of_expr_res e =
  e |> tau_of_expr |> Result.of_option ~error:ExpectedTau

let expr_of_term = function
  | `Unit -> Error ExpectedTerm
  | t -> Ok t

let mismatch t1 t2 = Mismatch ((t1 :> expr), (t2 :> expr))

let assert_eq ~exp got =
  if Poly.equal got exp then Ok () else Error (Mismatch (got, exp))

let assert_bool = assert_eq ~exp:`Bool