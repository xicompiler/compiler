open Core
include Definitions
include Conversions
module Tau = Tau
include TypeError

let assert_array = function
  | `Array _ -> Ok ()
  | `Poly | `Int | `Bool | `Tuple _ -> Error ExpectedArray

let assert_unit = function
  | `Unit -> Ok ()
  | `Poly | `Int | `Bool | `Array _ | `Tuple _ -> Error ExpectedUnit

let assert_void = function
  | `Void -> Ok ()
  | `Unit -> Error ExpectedVoid

module Error = TypeError
