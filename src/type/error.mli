open Core
open Definitions

(** An [t] is the type of a Xi type error *)
type t =
  | Unbound of string
  | Bound of string
  | ExpectedTau
  | ExpectedArray
  | ExpectedFn
  | ExpectedRecord
  | ExpectedUnit
  | ExpectedVoid
  | FnMismatch of string
  | RecordMismatch of string
  | OpMismatch
  | ExprMismatch of expr * expr
  | StmtMismatch of stmt * stmt
  | CountMismatch
  | IllegalArrayDecl
  | UnboundIntf of string
  | IllegalBreak
  | UnboundField of string
  | RepeatField of string

include Util.Stringable.S with type t := t

(** [Positioned] represents an error with a position *)
module Positioned : sig
  type nonrec t = t Position.Error.t
  (** an [error] represents a type error with an associated position *)

  type nonrec 'a result = ('a, t) result
  (** An ['a result] is either [Ok 'a] or a semantic error *)

  val expr_mismatch :
    Position.t -> expect:[< expr ] -> got:[< expr ] -> t
  (** [mismatch pos expect got] is [make ~pos ExprMismatch(expect,got)] *)

  val op_mismatch : Position.t -> t
  (** [op_mismatch pos] is an error occuring at positon [pos] describing
      a mismatch of operands *)

  val count_mismatch : Position.t -> t
  (** [count_mismatch pos] is [make ~pos CountMismatch] *)

  val illegal_arr_decl : Position.t -> t
  (** [illegal_arr_decl pos] is [make ~pos IllegalArrayDecl] *)

  val expected_unit : Position.t -> t
  (** [expected_unit pos] is [make ~pos ExpectedUnit] *)

  val expected_array : Position.t -> t
  (** [expected_array pos] is [make ~pos ExpectedArray] *)

  val illegal_break : Position.t -> t
  (** [illegal_break pos] is [make ~pos IllegalBreak] *)

  val unbound_field : Position.t -> string -> t
  (** [unbound_field pos] is [make ~pos UnboundField] *)

  val repeat_field : Position.t -> string -> t
  (** [repeat_field id] is [make ~pos RepeatField] *)
end

type nonrec 'a result = ('a, t) result
(** An ['a result] is either [Ok 'a] or [Error error] *)
