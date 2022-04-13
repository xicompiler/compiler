open! Core

(** [t] represents a condition code in x86 *)
type t =
  | Z
  | E
  | Nz
  | Ne
  | L
  | Le
  | G
  | Ge
  | B
  | Be
  | A
  | Ae

val of_cmp : Ir.Op.cmp -> t
(** [of_cmp op] is the condition code corresponding to comparison
    operator [cmp] *)
