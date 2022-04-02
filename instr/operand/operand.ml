open Core
open Option.Monad_infix
open Util.Fn

type dest =
  [ Reg.t
  | `Mem of Mem.t
  ]

type imm = int64

type t =
  [ dest
  | `Imm of imm
  ]

module Encoding = struct
  type rm = [ `RM of Reg.t * dest ]
  type mr = [ `MR of dest * Reg.t ]
  type mi = [ `MI of dest * imm ]
  type rmi = [ `RMI of Reg.t * dest * imm ]
end

module Reg = Reg
module Mem = Mem
