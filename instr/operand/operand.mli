open! Core

type imm = [ `Imm of Imm.t ]
(** [imm] represents an immediate operand *)

type t =
  [ Dest.t
  | imm
  ]
(** [t] is the type of an operand in x86 *)

type abstract =
  [ Dest.abstract
  | imm
  ]
(** [abstract] is the type of an abstract operand in x86 *)

module Encoding : module type of Encoding
(** [Encoding] represents an instruction encoding, as specified in the
    x86 manual *)

module Reg : module type of Reg
(** [Reg] represents a module in x86 *)

module Mem : module type of Mem
(** [Mem] represents a memory operand in x86 *)

module Imm : module type of Imm
(** [Imm] represents an immediate operand in x86 *)

module Dest : module type of Dest
(** [Dest] represents a destination operand in x86 *)
