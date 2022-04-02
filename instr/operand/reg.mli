type t =
  [ `Rax
  | `Rbx
  | `Rcx
  | `Rdx
  | `Rsi
  | `Rdi
  | `Rsp
  | `Rbp
  | `R9
  | `R10
  | `R11
  | `R12
  | `R13
  | `R14
  | `R15
  ]
(** [t] is the type of a register in x86 *)

include Util.Stringable.S with type t := t
