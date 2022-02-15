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

let rec to_string = function
  | Primitive Int -> "int"
  | Primitive Bool -> "bool"
  | Array { contents; _ } -> "[]" ^ to_string contents

let rec equal t1 t2 =
  match (t1, t2) with
  | Array t1', Array t2' -> equal t1'.contents t2'.contents
  | _ -> t1 = t2
