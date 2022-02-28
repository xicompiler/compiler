open Core

module type S = sig
  type 'a node

  type nonrec primitive =
    [ `Int
    | `Bool
    ]

  type t =
    [ primitive
    | `Array of t node
    ]
end

module Make (T1 : T1) = struct
  type 'a node = 'a T1.t

  type nonrec primitive =
    [ `Int
    | `Bool
    ]

  type t =
    [ primitive
    | `Array of t node
    ]
end

include Make (Monad.Ident)