type nonrec primitive =
  | Int
  | Bool

type t =
  | Primitive of primitive
  | Array of t

let rec to_string = function
  | Primitive p -> begin
      match p with
      | Int -> "int"
      | Bool -> "bool"
    end
  | Array t -> "[]" ^ to_string t
