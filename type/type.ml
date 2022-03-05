open Core
include Definitions
include Conversions

type context = Context.t

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

let assert_array = function
  | `Array _ -> Ok ()
  | `Poly
  | `Int
  | `Bool
  | `Tuple _ ->
      Error ExpectedArray

let assert_unit = function
  | `Unit -> Ok ()
  | `Poly
  | `Int
  | `Bool
  | `Array _
  | `Tuple _ ->
      Error ExpectedUnit

module Node = TypeNode
module Error = TypeError
