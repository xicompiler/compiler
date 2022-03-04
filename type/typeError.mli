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
  | ExpectedUnit
  | ArgMismatch
  | OpMismatch
  | Mismatch of expr * expr
  | StmtMismatch of stmt * stmt
  | CountMismatch
  | IllegalArrayDecl

(** [Positioned] represents an error with a position *)
module Positioned : sig
  include module type of Position.Error

  type nonrec error = error t
  (** an [error] represents a type error with an associated position *)

  type nonrec 'a result = ('a, error) result
  (** An ['a result] is either [Ok 'a] or a semantic error *)

  val count_mismatch : Position.t -> error
  (** [count_mismatch pos] is [make ~pos CountMismatch]*)

  val illegal_arr_decl : Position.t -> error
  (** [illegal_arr_decl pos] is [make ~pos IllegalArrayDecl] *)

  val expected_unit : Position.t -> error
  (** [expected_unit pos] is [make ~pos ExpectedUnit] *)
end

type nonrec 'a result = ('a, error) result
(** An ['a result] is either [Ok 'a] or [Error error] *)
