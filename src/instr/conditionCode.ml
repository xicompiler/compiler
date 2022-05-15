open Core
open Util.Fn

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
[@@deriving variants, sexp]

let of_cmp : Ir.Op.cmp -> t = function
  | `Eq -> E
  | `Geq -> Ge
  | `Gt -> G
  | `Leq -> Le
  | `Lt -> L
  | `Neq -> Ne
  | `UGeq -> Ae
  | `UGt -> A
  | `ULeq -> Be
  | `ULt -> B

let to_string = Variants.to_name >> String.lowercase
