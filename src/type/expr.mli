type t =
  [ Tau.t
  | `Tuple of Tau.t list
  ]
[@@deriving sexp_of]
(** [t] is the type of an expression node in Xi. *)

include Util.Stringable.S with type t := t

val to_tau : t -> Tau.t option
(** [to_tau e] is [Some t] if [e] is tau type [t] and [None] otherwise *)

val is_tau : t -> bool
(** [is_tau t] is [true] iff [t] is not [`Tuple _] *)

val equal : [< t ] -> [< t ] -> bool
(** [equal t1 t2] is [true] iff [t1] and [t2] represent the same
    expression type. *)

val is_array : [< t ] -> bool
(** [is_array t] is [true] iff [t] is an array type. *)
