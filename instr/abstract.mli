open Operand

type jmp =
  [ Dest.abstract
  | Ir.name
  ]
(** [jmp] is the type of an operand to an abstract [jmp] instruction *)

type mul =
  [ Dest.abstract
  | (Reg.abstract, Dest.abstract) Encoding.rm
  | (Reg.abstract, Dest.abstract) Encoding.rmi
  ]
(** [mul] is the type of an operand to an abstract [mul] or [imul]
    instruction *)

type t =
  < reg : Reg.abstract
  ; reg8 : Reg.Bit8.abstract
  ; dest : Dest.abstract
  ; operand : abstract
  ; jmp : jmp
  ; mul : mul >
  Generic.t
(** [t] is the type of an abstract assembly instruction *)
