open! Core

type ('a, 'b) t = 'a * 'b
(** An [('a, 'b) t] is a node storing key of type ['a] and data of type
    ['b] *)

val create : key:'a -> data:'b -> ('a, 'b) t
(** [create ~key ~data] is a node storing key [key] and data [data] *)

val key : ('a, 'b) t -> 'a
(** [key node] is the key stored in [node] *)

val data : ('a, 'b) t -> 'b
(** [data node] is the data stored within [node] *)

(** [Key] contains higher order functions for applied to the [key] of an
    entry *)
module Key : sig
  val fold : ('a, 'b) t -> f:('a -> 'c) -> 'c
  (** [fold entry ~f] is [f (key entry)] *)

  val map : ('a, 'b) t -> f:('a -> 'c) -> ('c, 'b) t
  (** [map entry ~f] is [entry] with its [key] equal to [fold entry ~f] *)
end

(** Analagous to [Key] *)
module Data : sig
  val fold : ('a, 'b) t -> f:('b -> 'c) -> 'c
  (** [fold entry ~f] is [f (data entry)] *)

  val set : ('a, 'b) t -> data:'c -> ('a, 'c) t
  (** [set e ~data] is [e] with its data set to [data] *)
end
