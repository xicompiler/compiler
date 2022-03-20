type t =
  [ `And
  | `Or
  ]
(** A [t] represents a logical binary operator *)

val eval : t -> bool -> bool -> bool
(** [eval op b1 b2] is [b1 && b2] if [op] is [`And] and [b1 || b2] if
    [op] is [`Or] *)
