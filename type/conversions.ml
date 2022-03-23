open Core
open Util.Result
open Definitions
open TypeError

let tau_of_expr = function `Tuple _ -> None | #tau as t -> Some t

let tau_of_expr_res e pos =
  e |> tau_of_expr
  |> Result.of_option ~error:(Positioned.make ~pos ExpectedTau)

let tau_list_of_term = function
  | `Unit -> []
  | `Tuple ts -> ts
  | #tau as t -> [ t ]

let term_of_tau_list = function
  | [] -> `Unit
  | [ t ] -> (t :> term)
  | lst -> `Tuple lst

let expr_of_term = function `Unit -> `Tuple [] | #expr as t -> t
