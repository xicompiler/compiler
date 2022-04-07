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
    ]
  (** [t] is the type of a 8-bit register in x68 *)

  type abstract =
    [ t
    | Ir.temp
    ]
  (** [abstract] is the type of an abstract 8-bit register *)
end

type t =
  [ Bit64.t
  | Bit8.t
  ]
(** [t] is the type of a register in x86 *)

type abstract =
  [ t
  | Ir.temp
  ]
(** [abstract] is the type of an abstract register *)

include Util.Stringable.S with type t := t
