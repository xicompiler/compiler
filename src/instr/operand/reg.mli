open Core

module Bit64 : sig
  type t =
    [ `rax
    | `rbx
    | `rcx
    | `rdx
    | `rsi
    | `rdi
    | `r8
    | `r9
    | `r10
    | `r11
    | `r12
    | `r13
    | `r14
    | `r15
    | `rsp
    | `rbp
    | `rip
    ]
  [@@deriving sexp, compare, equal, hash]
  (** [t] is the type of a 64-bit register in x68 *)

  val caller_save : [> t ] Sequence.t
  (** [caller_save] is the list of registers that are caller-save *)

  include Intable with type t := t
end

module Bit8 : sig
  type t =
    [ `ah
    | `al
    | `bh
    | `bl
    | `ch
    | `cl
    | `dh
    | `dl
    | `r8b
    ]
  (** [t] is the type of a 8-bit register in x68 *)

  val to_64_bit : [< t ] -> [> Bit64.t ]
  (** [to_64_bit r] is the 64 higher order bit register of [r] *)
end

type t =
  [ Bit64.t
  | Bit8.t
  ]
[@@deriving equal]
(** [concrete] is the type of a register in x86 *)

val to_64_bit : [< t ] -> [> t ]
(** [to_64_bit r] is the 64 higher order bit register of [r] *)

val to_8_bit : [< t ] -> [> Bit8.t ]
(** [to_8_bit r] is the 8 lower order bit register of [r] *)

include Util.Stringable.S with type t := t

(** [Abstract] is an abstract register *)
module Abstract : sig
  type t =
    [ Bit64.t
    | Ir.Temp.Virtual.t
    ]
  [@@deriving sexp, compare, equal, hash]
  (** [t] is the type of an abstract register *)

  include Util.Stringable.S with type t := t

  module Set : Set.S with type Elt.t = t
  (** [Set] is a set of virtual temporaries *)

  module Map : Map.S with type Key.t = t
  (** [Map] is a map with key type [t] *)

  module Table : Hashtbl.S with type key = t
  (** [Table] is a hashtable with key type [t] *)
end
