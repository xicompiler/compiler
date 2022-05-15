open Core

type 'a generic =
  [ `Mem of 'a Mem.generic
  | `Imm of Imm.t
  | Ir.name
  ]
[@@deriving sexp]

type t =
  [ Reg.t
  | Reg.t generic
  ]

let to_string : [< t ] -> string = function
  | #Reg.t as r -> Reg.to_string r
  | `Mem mem -> Mem.to_string mem
  | `Imm i -> Int64.to_string i
  | `Name l -> l

type concrete = t

module Abstract = struct
  type t =
    [ Reg.Abstract.t generic
    | Reg.Abstract.t
    ]
  [@@deriving sexp]

  (** [map_generic ~f op] applies [f] to every register in generic
      operand [op] *)
  let map_generic ~f = function
    | (`Imm _ | `Name _) as op -> op
    | `Mem mem -> `Mem (Mem.map mem ~f)

  let map (op : t) ~f =
    match op with
    | (`Imm _ | `Name _ | `Mem _) as op -> map_generic op ~f
    | #Reg.Abstract.t as r -> (f r :> t)

  let map_concrete (op : t) ~f =
    match op with
    | (`Imm _ | `Name _ | `Mem _) as op -> map_generic op ~f
    | #Reg.Abstract.t as r -> (f r :> concrete)

  let to_string : [< t ] -> string = function
    | #Reg.Abstract.t as r -> Reg.Abstract.to_string r
    | `Mem mem -> Mem.Abstract.to_string mem
    | `Imm i -> Int64.to_string i
    | `Name l -> l
end

module Reg = Reg
module Mem = Mem
module Imm = Imm
