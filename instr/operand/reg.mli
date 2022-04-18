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
  (** [to_64_bit r] are the 64 higher order bit register of [r] *)
end

type t =
  [ Bit64.t
  | Bit8.t
  ]
[@@deriving equal]
(** [t] is the type of a register in x86 *)

type concrete = t

include Util.Stringable.S with type t := t

(** [Abstract] is an abstract register *)
module Abstract : sig
  type t =
    [ concrete
    | Ir.Temp.Virtual.t
    ]
  (** [t] is the type of an abstract register *)

  include Util.Stringable.S with type t := t
end
