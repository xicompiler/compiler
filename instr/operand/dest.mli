type t =
  [ Reg.t
  | `Mem of Mem.t
  ]
(** A [t] is an operand that can be the destination of an instruction *)

type abstract =
  [ Reg.abstract
  | `Mem of Mem.abstract
  ]
(** [abstract] is the type of an abstract destination in x86 *)
