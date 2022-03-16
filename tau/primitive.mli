type t =
  [ `Int
  | `Bool
  ]
[@@deriving sexp_of]
(** A [t] is the type of a primitive value in Xi: either an integer or a
    boolean *)
