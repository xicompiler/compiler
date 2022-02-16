open Core

type nonrec primitive =
  | Int
  | Bool

type 'a t =
  | Primitive of primitive
  | Array of 'a array

and 'a array = {
  contents : 'a t;
  length : 'a option;
}

let make_array contents length = Array { contents; length }
