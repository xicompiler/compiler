type t =
  [ `Eq
  | `Neq
  ]
(** A [t] represents an equality operator *)

val eval : equal:('a -> 'a -> bool) -> t -> 'a -> 'a -> bool
(** [eval ~equal op x1 x2] is [equal x1 x2] if [op] is [`Eq] and
    [not (equal x1 x2)] if [op] is [`Neq]. *)
