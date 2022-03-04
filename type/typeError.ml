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

module Positioned = struct
  include Position.Error

  type nonrec error = error t
  type nonrec 'a result = ('a, error) result
end

type nonrec 'a result = ('a, error) result