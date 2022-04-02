open Core

type label = string
type operand = Operand.t

open Operand

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

module Operand = Operand
