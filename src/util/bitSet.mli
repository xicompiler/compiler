open! Core

type t
(** [t] is a set represented by a bit vector *)

val empty : t
(** [empty] is the empty set *)

val universe : t
(** [universe] is the set [{0, 1, ... , Int.num_bits - 1}] *)

val is_empty : t -> bool
(** [is_empty s] is [true] iff [s] is the empty set *)

val inter : t -> t -> t
(** [inter s1 s2] is the intersection of sets [s1] and [s2] *)

val union : t -> t -> t
(** [union s1 s2] is the union of sets [s1] and [s2] *)

val diff : t -> t -> t
(** [diff s1 s2] is the set difference [s1 - s2] *)

val complement : t -> t
(** [complement s] is [diff universe s] *)

val add : int -> t -> t
(** [add i s] is [s] with element [i] added. Raises: [Invalid_argument]
    if [i < 0] or [i >= Int.num_bits]. *)

val range : max:int -> t
(** [range ~max] is the set [{0, ..., max - 1}] Raises:
    [Invalid_argument] if [max < 0] or [max > Int.num_bits]*)

val remove : int -> t -> t
(** [remove i s] is [s] with element [i] removed. Raises:
    [Invalid_argument] if [i < 0] or [i > Int.num_bits]. *)

val min : t -> int option
(** [min s] is [Some min] where [min] is the minimum element present in
    [s] or [None] if [s] is empty *)

val pop_min : t -> (int * t) option
(** [pop_min s] is [Some (i, s')] where [i] is [i] the minimum element
    present in [s] and [Some s'] is [remove s i], or [None] if [s] is
    empty *)

val elements : t -> int list
(** [elements s] is a list of the elements in [s] in an unspecified
    order *)
