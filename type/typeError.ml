open Core
open Definitions

type error =
  | Unbound of string
  | Bound of string
  | ExpectedTau
  | ExpectedArray
  | ExpectedFn
  | ExpectedUnit
  | ExpectedVoid
  | FnMismatch of string
  | OpMismatch
  | Mismatch of expr * expr
  | StmtMismatch of stmt * stmt
  | CountMismatch
  | IllegalArrayDecl
  | UnboundIntf of string

let to_string = function
  | Unbound s -> "Unbound identifier: " ^ s
  | Bound s -> "Identifier already bound in context: " ^ s
  | ExpectedTau -> "Expected type tau"
  | ExpectedArray -> "Expected an array"
  | ExpectedFn -> "Expected a function"
  | ExpectedUnit -> "Expected type unit"
  | ExpectedVoid -> "Expected type void"
  | FnMismatch s ->
      "Function declaration " ^ s ^ " does not match signature"
  | OpMismatch -> "Operation mismatch"
  | Mismatch (e1, e2) ->
      let str1 = Expr.to_string e1 in
      let str2 = Expr.to_string e2 in
      Printf.sprintf "Expression mismatch, types %s and %s" str1 str2
  | StmtMismatch (s1, s2) ->
      let str1 = Stmt.to_string s1 in
      let str2 = Stmt.to_string s2 in
      Printf.sprintf "Statement mismatch, types %s and %s" str1 str2
  | CountMismatch -> "Argument count mismatch"
  | IllegalArrayDecl -> "Illegal array declaration"
  | UnboundIntf s -> "Unbound interface: " ^ s

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