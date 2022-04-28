type t =
  [ `And
  | `Or
  ]
(** A [t] represents a logical binary operator *)

val eval : t -> bool -> bool -> bool
(** [eval op b1 b2] is [b1 && b2] if [op] is [`And] and [b1 || b2] if
    [op] is [`Or] *)

val eval_bits : t -> int64 -> int64 -> int64
(** [eval op i1 i2] is [b1 land b2] if [op] is [`And] and [b1 lor b2] if
    [op] is [`Or] *)
