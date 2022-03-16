open Core

type base =
  [ `Int of int64
  | `Bool of bool
  ]
[@@deriving variants]

type t =
  [ base
  | `Char of Uchar.t
  ]

let cast = function
  | `Char u -> `Int (u |> Uchar.to_scalar |> Int64.of_int)
  | #base as b -> b

let typeof = function `Int _ | `Char _ -> `Int | `Bool _ -> `Bool
