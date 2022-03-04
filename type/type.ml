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

let tau_list_of_term = function
  | `Unit -> []
  | `Tuple ts -> ts
  | (`Int | `Bool | `Array _) as t -> [ t ]

let expr_of_term = function
  | `Unit -> `Tuple []
  | `Tuple t -> `Tuple t
  | (`Int | `Bool | `Array _) as t -> t

let assert_array = function
  | `Array _ -> Ok ()
  | `Int
  | `Bool
  | `Tuple _ ->
      Error ExpectedArray

(* TODO add pos *)
let assert_eq_tau t1 t2 =
  let%bind t1 = tau_of_expr_res t1 in
  let%bind t2 = tau_of_expr_res t2 in
  let error () = TypeNode.Expr.mismatch_sub ~expect:t1 t2 in
  ok_if_true_lazy (Tau.equal t1 t2) ~error

let assert_unit = function
  | `Unit -> Ok ()
  | `Int
  | `Bool
  | `Array _
  | `Tuple _ ->
      Error ExpectedUnit

module Node = TypeNode
module Error = TypeError