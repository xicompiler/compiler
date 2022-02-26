(** [S] is the type of a type *)
module type S = sig
  module Node : Node.S
  (** [Node] wraps the array type *)

  type nonrec primitive =
    [ `Int
    | `Bool
    ]
  (** A [primitive] is the type of a primitive value in Xi: either an
      integer or a boolean *)

  type t =
    [ primitive
    | `Array of t Node.t
    ]
  (** A type in Xi is either a primitive type or an array of a type,
      where an Array is represented by a pair (contents, length) *)
end

(** [Make (Node)] is an [S] where the the array type is wrapped in
    [Node] *)
module Make (Node : Node.S) : S with module Node = Node

include S with module Node = Node.Ident
