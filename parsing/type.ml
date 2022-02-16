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

let rec sexp_of_t = function
  | Primitive Int -> Sexp.Atom "int"
  | Primitive Bool -> Sexp.Atom "bool"
  | Array { contents; _ } ->
      Sexp.List [ Sexp.Atom "[]"; sexp_of_t contents ]
