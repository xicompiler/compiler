open Error
include Definitions

let assert_unit = function
  | `Unit -> Ok ()
  | `Bot | `Int | `Bool | `Array _ | `Tuple _ -> Error ExpectedUnit

let assert_void = function
  | `Void -> Ok ()
  | `Unit -> Error ExpectedVoid

module Error = Error
