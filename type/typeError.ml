open Core
open Definitions

type error =
  | Unbound of string
  | Bound of string
  | IdMismatch of string
  | ExpectedTau
  | ExpectedArray
  | ExpectedFn
  | ExpectedTerm
  | ExpectedUnit
  | ArgMismatch
  | OpMismatch
  | Mismatch of expr * expr
  | StmtMismatch of stmt * stmt
  | CountMismatch
  | IllegalArrayDecl

module Positioned = struct
  include Position.Error

  type nonrec error = error t
  type nonrec 'a result = ('a, error) result

  let mismatch pos ~expect got =
    let cause = Mismatch ((expect :> expr), (got :> expr)) in
    make ~pos cause

  let count_mismatch pos = make ~pos CountMismatch
  let illegal_arr_decl pos = make ~pos IllegalArrayDecl
  let expected_unit pos = make ~pos ExpectedUnit
  let expected_array pos = make ~pos ExpectedArray
end

type nonrec 'a result = ('a, error) result