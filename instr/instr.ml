open Core
open Operand

type jmp =
  [ Dest.t
  | Ir.name
  ]

type mul =
  [ Dest.t
  | (Reg.t, Dest.t) Encoding.rm
  | (Reg.t, Dest.t) Encoding.rmi
  ]

type t =
  < reg : Reg.t
  ; reg8 : Reg.Bit8.t
  ; dest : Dest.t
  ; operand : Operand.t
  ; jmp : jmp
  ; mul : mul >
  Generic.t

module Operand = Operand
module Generic = Generic
module Abstract = Abstract
module ConditionCode = ConditionCode
