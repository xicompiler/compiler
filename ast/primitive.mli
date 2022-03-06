(** A [primitive] represents a primitive int, bool, or char value in Xi *)
type primitive =
  | Int of Int64.t
  | Bool of bool
  | Char of Uchar.t
