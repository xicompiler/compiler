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
  | ExpectedUnit
  | ArgMismatch
  | OpMismatch
  | Mismatch of expr * expr
  | CountMismatch
  | IllegalArrayDecl

module Positioned = struct
  include Position.Error

  type nonrec error = error t
  type nonrec 'a result = ('a, error) result

  let count_mismatch pos = make ~pos CountMismatch
  let illegal_arr_decl pos = make ~pos IllegalArrayDecl
  let expected_unit pos = make ~pos ExpectedUnit
end

type nonrec 'a result = ('a, error) result