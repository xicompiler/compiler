open! Core

type ('reg, 'dest) rm = [ `RM of 'reg * 'dest ]
type ('reg, 'dest) mr = [ `MR of 'reg * 'dest ]
type 'dest mi = [ `MI of 'dest * Imm.t ]
type ('reg, 'dest) rmi = [ `RMI of 'reg * 'dest * Imm.t ]
