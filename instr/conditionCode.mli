open! Core

(** [t] represents a condition code in [Xi] *)
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
