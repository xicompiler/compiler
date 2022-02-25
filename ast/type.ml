module type S = sig
  module Node : Node.S

  type nonrec primitive =
    | Int
    | Bool

  type t =
    | Primitive of primitive
    | Array of t Node.t
end

module Make (Node : Node.S) = struct
  module Node = Node

  type nonrec primitive =
    | Int
    | Bool

  type t =
    | Primitive of primitive
    | Array of t Node.t
end