open Core

type t = [ `Temp of string ]
(** [t] represents the type of a temporary variable in Xi *)

module Virtual : sig
  type nonrec t =
    [ t
    | `Rv of int
    | `Arg of int
    ]
  [@@deriving equal, sexp, compare, hash]
  (** [VirtualReg.t] is the type of a virtual register in abstract
      assembly *)

  val rv : int -> string
  (** [rv n] is a temporary corresponding to the [n]th virtual return
      value register *)

  val arg : int -> string
  (** [arg n] is a temporary corresponding to the [n]th virtual argument
      value register *)

  val to_string : [< t ] -> string
  (** [to_string t] is the string representation of [t] *)

  module Set : Set.S with type Elt.t = t
  (** [Set] is a set of virtual temporaries *)

  module Table : Hashtbl.S with type key = t
  (** [Table] is a table with key type [t] *)

  module Map : Map.S with type Key.t = t
  (** [Map] is a map with key type [t] *)
end
