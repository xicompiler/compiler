open Core

(** [Make (Key)] is a [Abstract.S] storing vertices of type [Key.t] *)
module Make (Key : Abstract.Key) : Abstract.S with module Key = Key
