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

let lub_stmt s1 s2 = lub (TypeNode.Stmt.typ s1) (TypeNode.Stmt.typ s2)

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
  let exp = (exp :> expr) in
  let got = (got :> expr) in
  if Poly.equal got exp then Ok () else Error (Mismatch (got, exp))

let assert_bool = assert_eq ~exp:`Bool

module type Context = sig
  include Node.S

  type typ
  type context
  type nonrec 'a result = ('a, error Position.error) Result.t

  val context : 'a t -> context
  val typ : 'a t -> typ
  val make : 'a -> ctx:context -> typ:typ -> 'a t
end

module Node = TypeNode