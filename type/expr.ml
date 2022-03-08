open Core
open Sexplib.Std

type t =
  [ Tau.t
  | `Tuple of Tau.t list
  ]
[@@deriving sexp_of]

let to_string = function
  | #Tau.t as t -> Tau.to_string t
  | `Tuple ts ->
      ts
      |> List.map ~f:Tau.to_string
      |> String.concat ~sep:", " |> Printf.sprintf "(%s)"

let equal t1 t2 =
  match (t1, t2) with
  | `Tuple ts1, `Tuple ts2 -> List.equal Tau.equal ts1 ts2
  | `Tuple _, _ | _, `Tuple _ -> false
  | (#Tau.t as t1), (#Tau.t as t2) -> Tau.equal t1 t2
