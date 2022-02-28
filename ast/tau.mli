open Core

(** [S] is the type of a type *)
module type S = sig
  type 'a node
  (** [node] wraps the array type *)

  type nonrec primitive =
    [ `Int
    | `Bool
    ]
  (** A [primitive] is the type of a primitive value in Xi: either an
      integer or a boolean *)

  type t =
    [ primitive
    | `Array of t node
    ]
  (** A type in Xi is either a primitive type or an array of a type,
      where an Array is represented by a pair (contents, length) *)
end

(** [Make (Node)] is an [S] where the the array type is wrapped in
    [Node] *)
module Make (T1 : T1) : S with type 'a node = 'a T1.t

include S with type 'a node = 'a
