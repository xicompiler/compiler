open Core
open Definitions

type t =
  | Unbound of string
  | Bound of string
  | ExpectedTau
  | ExpectedArray
  | ExpectedFn
  | ExpectedUnit
  | ExpectedVoid
  | FnMismatch of string
  | OpMismatch
  | ExprMismatch of expr * expr
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
  | ExprMismatch (e1, e2) ->
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
  type nonrec t = t Position.Error.t
  type nonrec 'a result = ('a, t) result

  let expr_mismatch pos ~expect ~got =
    let cause = ExprMismatch ((expect :> expr), (got :> expr)) in
    Position.Error.create ~pos cause

  let count_mismatch pos = Position.Error.create ~pos CountMismatch
  let op_mismatch pos = Position.Error.create ~pos OpMismatch
  let illegal_arr_decl pos = Position.Error.create ~pos IllegalArrayDecl
  let expected_unit pos = Position.Error.create ~pos ExpectedUnit
  let expected_array pos = Position.Error.create ~pos ExpectedArray
end

type nonrec 'a result = ('a, t) result
