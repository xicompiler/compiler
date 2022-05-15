type regs =
  [ `r8
  | `r9
  | `r10
  ]
[@@deriving equal]
(** [regs] is the type of the shuttling registers for a single
    instruction *)

val shuttle : [> regs ] list
(** [shuttle] is a list of shuttling registers. The number of shuttling
    registers should be greater or equal to the maximum possible number
    of operands in an instruction *)

type t = {
  regs : Reg.t list;
  mapping : Reg.t Reg.Abstract.Map.t;
}
[@@deriving sexp]
(** [t] represents the shuttling registers and mapping from abstract
    register to concrete shuttling register *)

val set : t -> Reg.Abstract.t -> Reg.t * t
(** [set shuttle abstract] is [(concrete, shuttle')] where [concrete] is
    a free shuttling register for [abstract] and [shuttle'] is the
    updated shuttle *)

val set_bit8 : t -> Reg.Abstract.t -> [> Reg.t ] * t
(** [set_bit8 shuttle abstract] is [(concrete, shuttle')] where
    [concrete] is a free shuttling register for 8-bit [abstract] and
    [shuttle'] is the updated shuttle *)

val find_default : t -> Reg.Abstract.t -> Reg.t
(** [find_default shuttle abstract] is [shuttle]'s mapped value of
    [abstract], or [abstract] if it doesn't exist *)

val find_bit8_exn : t -> Reg.Abstract.t -> [> Reg.Bit8.t ]
(** [find_bit8_exn shuttle abstract] is [shuttle]'s mapped value of
    8-bit [abstract], or raises an exception if it doesn't exist *)

val empty : t
(** [empty] is an initialized shuttle with an empty mapping and all
    shuttle registers available *)
