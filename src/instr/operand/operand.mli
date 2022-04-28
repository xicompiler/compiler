open Core

type 'a generic =
  [ Reg.t
  | `Mem of 'a Mem.generic
  | `Imm of Imm.t
  | Ir.name
  ]
(** ['a generic] is the type of an operand with register type ['a] *)

type t = Reg.t generic
(** [t] is the type of an operand in x86 *)

include Util.Stringable.S with type t := t

(** [Abstract] represents an abstract operand in x86 *)
module Abstract : sig
  type t =
    [ Reg.Abstract.t generic
    | Ir.Temp.Virtual.t
    ]
  (** [t] is the type of an abstract operand in x86 *)

  include Util.Stringable.S with type t := t
end

module Reg : module type of Reg
(** [Reg] represents a module in x86 *)

module Mem : module type of Mem
(** [Mem] represents a memory operand in x86 *)

module Imm : module type of Imm
(** [Imm] represents an immediate operand in x86 *)
