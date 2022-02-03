type nonrec primitive =
  | Int
  | Bool

type t =
  | Primitive of primitive
  | Array of t