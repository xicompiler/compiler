open Core

type 'a generic =
  [ `Mem of 'a Mem.generic
  | `Imm of Imm.t
  | Ir.name
  ]

type t =
  [ Reg.t
  | Reg.t generic
  ]

let to_string : [< t ] -> string = function
  | #Reg.t as r -> Reg.to_string r
  | `Mem mem -> Mem.to_string mem
  | `Imm i -> Int64.to_string i
  | `Name l -> l

module Abstract = struct
  type t =
    [ Reg.Abstract.t generic
    | Reg.Abstract.t
    ]

  let map (op : t) ~f : [> 'a generic ] =
    match op with
    | (`Imm _ | `Name _) as op -> op
    | `Mem mem -> `Mem (Mem.map mem ~f)
    | #Reg.Abstract.t as r -> f r

  let to_string : [< t ] -> string = function
    | #Reg.Abstract.t as r -> Reg.Abstract.to_string r
    | `Mem mem -> Mem.Abstract.to_string mem
    | `Imm i -> Int64.to_string i
    | `Name l -> l
end

module Reg = Reg
module Mem = Mem
module Imm = Imm
