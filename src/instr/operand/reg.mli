open Core

module Bit64 : sig
  type t =
    [ `rax
    | `rbx
    | `rcx
    | `rdx
    | `rsi
    | `rdi
    | `rsp
    | `rbp
    | `rip
    | `r8
    | `r9
    | `r10
    | `r11
    | `r12
    | `r13
    | `r14
    | `r15
    ]
  (** [t] is the type of a 64-bit register in x68 *)
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

  val to_64_bit : t -> [> Bit64.t ]
  (** [to_64_bit r] is the 64 higher order bit register of [r] *)
end

type concrete =
  [ Bit64.t
  | Bit8.t
  ]
(** [concrete] is the type of a register in x86 *)

type t = concrete [@@deriving equal]
(** [t] is the type of a register in x86 *)

val to_64_bit : t -> [> t ]
(** [to_64_bit r] is the 64 higher order bit register of [r] *)

val to_8_bit : t -> [> Bit8.t ]
(** [to_8_bit r] is the 8 lower order bit register of [r] *)

include Util.Stringable.S with type t := t

(** [Abstract] is an abstract register *)
module Abstract : sig
  type t =
    [ concrete
    | Ir.Temp.Virtual.t
    ]
  (** [t] is the type of an abstract register *)

  val to_64_bit : t -> t
  (** [to_64_bit r] is the 64 higher order bit register of [r] *)

  include Util.Stringable.S with type t := t

  module Set : Set.S with type Elt.t = t
  (** [Set] is a set of virtual temporaries *)

  module Map : Map.S with type Key.t = t
  (** [Map] is a map with key type [t] *)

  module Table : Hashtbl.S with type key = t
  (** [Table] is a hashtable with key type [t] *)
end
