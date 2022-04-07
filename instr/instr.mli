open Core
open Operand

type jmp =
  [ Dest.t
  | Ir.name
  ]
(** [jmp] is the type of an operand to a concrete [jmp] instruction *)

type mul =
  [ Dest.t
  | (Reg.t, Dest.t) Encoding.rm
  | (Reg.t, Dest.t) Encoding.rmi
  ]
(** [mul] is the type of an operand to a concrete [mul] or [imul]
    instruction *)

type t =
  < reg : Reg.t
  ; reg8 : Reg.Bit8.t
  ; dest : Dest.t
  ; operand : Operand.t
  ; jmp : jmp
  ; mul : mul >
  Generic.t
(** [t] is the type of a concrete assembly instruction *)

(** [Operand] represents an operand to an instruction *)
module Operand : module type of struct
  include Operand
end

(** [Generic] represents a generic assembly instruction, parameterized
    over operand types *)
module Generic : module type of struct
  include Generic
end

(** [Abstract] represents an abstract assembly instruction *)
module Abstract : module type of struct
  include Abstract
end

(** [ConditionCode] is a condition code in x86 *)
module ConditionCode : module type of struct
  include ConditionCode
end
