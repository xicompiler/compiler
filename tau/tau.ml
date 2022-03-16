open Core

type primitive = Primitive.t [@@deriving sexp_of]

type t =
  [ primitive
  | `Poly
  | `Array of t
  ]
[@@deriving sexp_of]

let rec to_string = function
  | `Int -> "Int"
  | `Bool -> "Bool"
  | `Poly -> "Poly"
  | `Array t -> "Array " ^ to_string t

let rec equal t1 t2 =
  match (t1, t2) with
  | `Poly, _ | _, `Poly | `Int, `Int | `Bool, `Bool -> true
  | `Array t1', `Array t2' -> equal t1' t2'
  | _ -> false

let is_array = function
  | `Array _ -> true
  | `Poly | `Int | `Bool -> false
