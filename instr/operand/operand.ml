open Core

type 'a generic =
  [ Reg.t
  | `Mem of 'a Mem.generic
  | `Imm of Imm.t
  | Ir.name
  ]

type t = Reg.t generic

let to_string : [< t ] -> string = function
  | #Reg.t as r -> Reg.to_string r
  | `Mem mem -> Mem.to_string mem
  | `Imm i -> Int64.to_string i
  | `Name l -> l

module Abstract = struct
  type t =
    [ Reg.Abstract.t generic
    | Ir.Temp.Virtual.t
    ]

  let to_string : [< t ] -> string = function
    | #Reg.t as r -> Reg.to_string r
    | `Mem mem -> Mem.Abstract.to_string mem
    | `Imm i -> Int64.to_string i
    | `Name l -> l
    | #Ir.Temp.Virtual.t as t -> Ir.Temp.Virtual.to_string t
end

module Reg = Reg
module Mem = Mem
module Imm = Imm
