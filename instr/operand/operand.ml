open! Core

type imm = [ `Imm of Imm.t ]

type t =
  [ Dest.t
  | imm
  ]

type abstract =
  [ Dest.abstract
  | imm
  ]

module Encoding = Encoding
module Reg = Reg
module Mem = Mem
module Imm = Imm
module Dest = Dest
