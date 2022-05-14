open! Core

type t
(** [t] represents a mapping from temporaries to stack locations *)

val create : unit -> t
(** [create gen] is a fresh map *)

val find : t -> Ir.Temp.Virtual.t -> Mem.Abstract.t
(** [find m t] is a pair the location of temporary [t] on the stack. One
    is allocated if it is not already *)

val count : t -> int
(** [count m] is the number of stack allocations allocated using [m] *)
