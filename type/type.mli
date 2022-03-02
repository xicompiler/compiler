open Core

type tau = Tau.t
(** [tau] is either a primitive or an array type. *)

type expr =
  [ tau
  | `Tuple of tau list
  ]
(** [expr] is the type of an expression node in Xi. *)

type term =
  [ expr
  | `Unit
  ]
(** [kind] is a type in procedures, functions, and multiple assignments. *)

type stmt =
  [ `Unit
  | `Void
  ]
(** [stmt] is the type of the outcome of evaluating a statement. *)

(** [id] is a type used in an environment entry. *)
type id =
  | Var of tau
  | Fn of term * term

(** An [error] is the type of a Xi type error *)
type error =
  | Unbound
  | Bound of string
  | IdMismatch of string
  | ExpectedTau
  | ExpectedArray
  | ExpectedFun
  | ExpectedTerm
  | ArgMismatch
  | OpMismatch
  | Mismatch of expr * expr

type nonrec 'a result = ('a, error) result
(** An ['a result] is either [Ok 'a] or [Error error] *)

type context = id Context.t
(** [context] is the type of a static typing context *)

val lub : term -> term -> term
(** [lub t1 t2] is [`Void] iff both of [t1] and [t2] are [`Void] and
    [`Unit] otherwise *)

(** [Context] is the type of a typing context *)
module Context : sig
  include module type of Context

  val assert_unbound : id:string -> context -> unit result
  (** [assert_unbound ~id ctx] is [Ok ()] if [id] is unbound in [ctx]
      and [Error Bound] otherwise *)

  (** [Fn] represents a typing context present within a function body *)
  module Fn : sig
    type t
    (** [t] represents the typing context found within a function. *)

    val context : t -> context
    (** [context fn_ctx] is the typing context found within [fn_ctx] *)

    val ret : t -> term
    (** [ret fn_ctx] is the return type of the function represented by
        [fn_ctx] *)

    val assert_unbound : id:string -> t -> unit result
    (** [assert_unbound ~id ctx] is [Ok ()] if [id] is unbound in [ctx]
        and [Error Bound] otherwise *)
  end

  type fn = Fn.t
  (** [fn] is an alias for [Fn.t] *)
end

module Tau : module type of Tau
(** export [Tau] for use by Menhir *)

val tau_of_expr : expr -> tau option
(** [tau_of_expr e] is [Some t] if [e] is tau type [t] and [None]
    otherwise *)

val assert_eq : exp:expr -> expr -> unit result
(** [assert_eq got ~exp] is [Ok ()] if [got] and [exp] represent the
    same type and [Error Mismatch] otherwise. *)

val assert_bool : expr -> unit result
(** [assert_bool e] is [Ok ()] if [e] is the boolean type and
    [Error Mismatch] otherwise *)

val tau_of_expr_res : expr -> tau result
(** [tau_of_expr_res e] is [Ok t] if [e] is tau type [t] and
    [Error ExpectedTau] otherwise *)

val mismatch : [< expr ] -> [< expr ] -> error
(** [mismatch t1 t2] is [Mismatch (t1 :> expr, t2 :> expr)] *)
