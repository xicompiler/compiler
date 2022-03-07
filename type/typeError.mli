open Core
open Definitions

(** An [error] is the type of a Xi type error *)
type error =
  | Unbound of string
  | Bound of string
  | ExpectedTau
  | ExpectedArray
  | ExpectedFn
  | ExpectedUnit
  | FnMismatch of string
  | OpMismatch
  | Mismatch of expr * expr
  | StmtMismatch of stmt * stmt
  | CountMismatch
  | IllegalArrayDecl
  | UnboundIntf of string

val to_string : error -> string

(** [Positioned] represents an error with a position *)
module Positioned : sig
  include module type of struct
    include Position.Error
  end

  type nonrec error = error t
  (** an [error] represents a type error with an associated position *)

  type nonrec 'a result = ('a, error) result
  (** An ['a result] is either [Ok 'a] or a semantic error *)

  val mismatch : Position.t -> expect:[< expr ] -> [< expr ] -> error
  (** [mismatch pos expect got] is [make ~pos Mismatch(expect,got)] *)

  val count_mismatch : Position.t -> error
  (** [count_mismatch pos] is [make ~pos CountMismatch] *)

  val illegal_arr_decl : Position.t -> error
  (** [illegal_arr_decl pos] is [make ~pos IllegalArrayDecl] *)

  val expected_unit : Position.t -> error
  (** [expected_unit pos] is [make ~pos ExpectedUnit] *)

  val expected_array : Position.t -> error
  (** [expected_array pos] is [make ~pos ExpectedArray] *)
end

type nonrec 'a result = ('a, error) result
(** An ['a result] is either [Ok 'a] or [Error error] *)
