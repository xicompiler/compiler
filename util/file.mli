open Core

val concat : dir:string -> string -> string
(** [concat ~dir file] is the concatenation of [dir] and [file] as a
    path *)

val xi : string -> string
(** [xi s] concats ".xi" to [s] *)

val ixi : string -> string
(** [ixi s] concats ".ixi" to [s] *)
