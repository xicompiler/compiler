open Core
open Subtype

type t
(** [t] represents an IR symbol generator where temporary supertype *)

val create : unit -> t
(** [create ()] is a fresh symbol generator, capable of generating fresh
    labels and fresh temps *)

val rv : int -> [> temp ]
(** [rv n] is a temporary corresponding to the [n]th virtual return
    value register *)

val rv1 : [> temp ]
(** [rv1] is the first virtual return register *)

val arg : int -> [> temp ]
(** [arg n] is a temporary corresponding to the [n]th virtual argument
    value register *)

(** [Temp] is a module used for generating fresh temporaries *)
module Temp : sig
  val fresh : t -> [> temp ]
  (** [fresh gen] is a fresh temp of type [t] *)

  val fresh2 : t -> ([> temp ] as 'a) * 'a
  (** [fresh2 gen] is a pair of fresh temps *)

  val fresh3 : t -> ([> temp ] as 'a) * 'a * 'a
  (** [fresh3 gen] is a triple of fresh temps *)
end

(** [Gen] is the module type of a generated symbol *)
module type Gen = sig
  type sym
  (** [sym] is the type of a generated symbol *)

  val generator : t -> unit -> sym
  (** [generator gen] is the generator function used to create fresh
      labels *)

  val fresh : t -> sym
  (** [fresh gen] is a fresh value of type [t] *)

  val fresh2 : t -> sym * sym
  (** [fresh2 gen] is a pair of fresh labels *)

  val fresh3 : t -> sym * sym * sym
  (** [fresh3 gen] is a triple of fresh labels *)
end

module type LabelGen = Gen with type sym := label
(** [LabelGen] is the type of a label generator *)

module Label : LabelGen
(** [Label] is a module used for generating fresh labels *)

module Global : LabelGen
(** [Global] is a module used for generating fresh globals *)
