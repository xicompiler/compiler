open Core
include Util.Result
include Definitions
include TypeError

let tau_of_expr = function
  | (`Int | `Bool | `Array _) as t -> Some t
  | `Tuple _ -> None

let tau_of_expr_res e pos =
  e |> tau_of_expr
  |> Result.of_option ~error:(Positioned.make ~pos ExpectedTau)

let tau_list_of_term = function
  | `Unit -> []
  | `Tuple ts -> ts
  | (`Int | `Bool | `Array _) as t -> [ t ]

let expr_of_term = function
  | `Unit -> `Tuple []
  | (`Int | `Bool | `Array _ | `Tuple _) as t -> t
