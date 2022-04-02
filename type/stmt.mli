open! Core

type t =
  [ `Unit
  | `Void
  ]
[@@deriving sexp_of]
(** [t] is the type of the outcome of evaluating a statement. *)

include Util.Stringable.S with type t := t

val lub : t -> t -> t
(** [lub t1 t2] is [`Void] iff both of [t1] and [t2] are [`Void] and
    [`Unit] otherwise *)
