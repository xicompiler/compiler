open Core
open Definitions

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