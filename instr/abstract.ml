open Operand

type jmp =
  [ Dest.abstract
  | Ir.name
  ]

type mul =
  [ Dest.abstract
  | (Reg.abstract, Dest.abstract) Encoding.rm
  | (Reg.abstract, Dest.abstract) Encoding.rmi
  ]

type t =
  < reg : Reg.abstract
  ; reg8 : Reg.Bit8.abstract
  ; dest : Dest.abstract
  ; operand : abstract
  ; jmp : jmp
  ; mul : mul >
  Generic.t
