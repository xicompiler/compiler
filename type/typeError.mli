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
  | ArgMismatch
  | OpMismatch
  | Mismatch of expr * expr

(** [Positioned] represents an error with a position *)
module Positioned : sig
  include module type of Position.Error

  type nonrec error = error t
  (** an [error] represents a type error with an associated position *)

  type nonrec 'a result = ('a, error) result
  (** An ['a result] is either [Ok 'a] or a semantic error *)
end

type nonrec 'a result = ('a, error) result
(** An ['a result] is either [Ok 'a] or [Error error] *)