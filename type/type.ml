open Core
include Definitions
include TypeError

type context = Context.context

module Tau = Tau
module Context = Context

let lub t1 t2 =
  match (t1, t2) with
  | `Void, `Void -> `Void
  | _ -> `Unit

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