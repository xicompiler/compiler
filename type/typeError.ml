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

module Position = struct
  type t = error Position.error
  type nonrec 'a result = ('a, t) result
end

type nonrec 'a result = ('a, error) result