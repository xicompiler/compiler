open Core
open Result.Let_syntax
include Definitions

type context = Context.context

module Tau = Tau
module Context = Context

module type Context = sig
  include Node.S

  type typ
  type context
  type nonrec 'a result = ('a, TypeError.error Position.error) result

  val context : 'a t -> context
  val typ : 'a t -> typ
  val make : 'a -> ctx:context -> typ:typ -> 'a t
end

include TypeError

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

(** [ok_if_true_lazy ~error b] is [Ok ()] if [b] is [true] and
    [Error (error ())] if [b] is false *)
let ok_if_true_lazy ~error b = if b then Ok () else Error (error ())

let assert_eq ~expect expr =
  let expect = (expect :> expr) in
  let got = TypeNode.Expr.typ expr in
  let error () =
    let pos = TypeNode.Expr.position expr in
    let cause = Mismatch (got, expect) in
    TypeError.Positioned.make ~pos cause
  in
  expect |> Poly.equal got |> ok_if_true_lazy ~error

let assert_bool expr = assert_eq ~expect:`Bool expr

let assert_array = function
  | `Array _ -> Ok ()
  | `Int
  | `Bool
  | `Tuple _ ->
      failwith "unimplemented"

let assert_eq_tau t1 t2 =
  let%bind t1 = tau_of_expr_res t1 in
  let%bind t2 = tau_of_expr_res t2 in
  ok_if_true_lazy (Tau.equal t1 t2) ~error:(fun () -> mismatch t1 t2)

module Node = TypeNode
module Error = TypeError