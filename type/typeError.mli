open Core
open Definitions

(** An [error] is the type of a Xi type error *)
type error =
  | Unbound of string
  | Bound of string
  | IdMismatch of string
  | ExpectedTau
  | ExpectedArray
  | ExpectedFun
  | ExpectedTerm
  | ArgMismatch
  | OpMismatch
  | Mismatch of expr * expr

type nonrec 'a result = ('a, error) result
(** An ['a result] is either [Ok 'a] or [Error error] *)
