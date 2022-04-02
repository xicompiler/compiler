type dest =
  [ Reg.t
  | `Mem of Mem.t
  ]
(** A [dest] is an operand that can be the destination of an instruction *)

type imm = int64
(** [imm] is the type of an immediate operand *)

type t =
  [ dest
  | `Imm of imm
  ]
(** [t] is the type of an operand in x86 *)

(** [Encoding] represents an instruction encoding, as specified in the
    x86 manual *)
module Encoding : sig
  type rm = [ `RM of Reg.t * dest ]
  (** [rm] is the type of a [RM] encoding, as described in the docs *)

  type mr = [ `MR of dest * Reg.t ]
  (** [mr] is the type of a [MR] encoding, as described in the docs *)

  type mi = [ `MI of dest * imm ]
  (** [mi] is the type of a [MI] encoding, as described in the docs *)

  type rmi = [ `RMI of Reg.t * dest * imm ]
  (** [rmi] is the type of a [RMI] encoding, as described in the docs *)
end

module Reg : module type of Reg
(** [Reg] represents a module in Xi *)

module Mem : module type of Mem
(** [Mem] represents a memory operand in x86 *)
