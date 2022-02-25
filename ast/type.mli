(** [S] is the type of a type *)
module type S = sig
  module Node : Node.S
  (** [Node] wraps the array type *)

  (** A [primitive] is the type of a primitive value in Xi: either an
      integer or a boolean *)
  type nonrec primitive =
    | Int
    | Bool

  (** A type in Xi is either a primitive type or an array of a type,
      where an Array is represented by a pair (contents, length) *)
  type t =
    | Primitive of primitive
    | Array of t Node.t
end

(** [Make (Node)] is an [S] where the the array type is wrapped in
    [Node] *)
module Make (Node : Node.S) : S with module Node = Node
