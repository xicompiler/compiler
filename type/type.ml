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
let equal_expr t1 t2 = Poly.equal (t1 :> expr) (t2 :> expr)

let assert_eq ~exp got =
  if Poly.equal got exp then Ok () else Error (Mismatch (got, exp))

let assert_bool = assert_eq ~exp:`Bool
let assert_eq_tau ~exp got = failwith "unimplemented"

let assert_array = function
  | `Array _ -> Ok ()
  | t -> failwith "unimplemented"

module Node = TypeNode
module Error = TypeError