open Core

type t = int64
(** [t] is an alias for [int64] *)

val of_char : char -> t
(** [of_char c] is the scalar value of [c] as an [int64] *)

val of_uchar : Uchar.t -> t
(** [of_uchar u] is the scalar value of [u] as an [int64] *)

val of_bool : bool -> t
(** [of_bool b] is [1L] iff [b] is [true] and [0L] otherwise *)
