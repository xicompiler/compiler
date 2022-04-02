open Core

type label = string
(** [label] is the type of an assembly label *)

(** [Operand] represents an operand to an instruction *)
module Operand : module type of struct
  include Operand
end

type operand = Operand.t
(** [operand] is the type of an instruction operand in x86 *)

open Operand

(** [t] is the type of an instruction in x86 *)
type t =
  | Jmp of [ dest | `Name of label ]
  | Jcc of ConditionCode.t * label
  | Push of operand
  | Pop of dest
  | Mov of dest * operand
  | IMul of [ dest | Encoding.rm | Encoding.rmi ]
  | Inc of dest
  | Dec of dest
  | Call of dest
  | IDiv of dest
  | Shl of dest * imm
  | Shr of dest * imm
  | Sar of dest * imm
  | Add of dest * operand
  | Sub of dest * operand
  | Lea of Reg.t * dest
  | Ret
