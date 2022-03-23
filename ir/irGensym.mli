open Core
open Subtype

type t
(** [t] represents an IR symbol generator where temporary supertype *)

val create : unit -> t
(** [create ()] is a fresh symbol generator, capable of generating fresh
    labels and fresh temps *)

(** [Temp] is a module used for generating fresh temporaries *)
module Temp : sig
  val fresh : t -> [> temp ]
  (** [fresh gen] is a fresh temp of type [t] *)

  val fresh2 : t -> ([> temp ] as 'a) * 'a
  (** [fresh2 gen] is a pair of fresh temps *)
end

(** [Label] is a module used for generating fresh labels *)
module Label : sig
  val fresh : t -> label
  (** [fresh gen] is a fresh value of type [t] *)

  val fresh2 : t -> label * label
  (** [fresh2 gen] is a pair of fresh labels *)

  val fresh3 : t -> label * label * label
  (** [fresh3 gen] is a triple of fresh labels *)

  val generator : t -> unit -> label
  (** [generator gen] is the generator function used to create fresh
      labels *)
end
