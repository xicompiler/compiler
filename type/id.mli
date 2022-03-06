open! Core

(** [t] is a type used in an environment entry. *)
type t =
  | Var of Tau.t
  | Fn of Term.t * Term.t
[@@deriving sexp_of]

val var : Tau.t -> t
(** [var t] is [Var t] *)

val fn : arg:[< Term.t ] -> ret:[< Term.t ] -> t
(** [fn ~arg ~ret] is [Fn (arg, ret)] *)

val proc : [< Term.t ] -> t
(** [proc arg] is [Fn (arg, `Unit)] *)

val fn_unit : [< Term.t ] -> t
(** [fn_unit ret] is [Fn (`Unit, ret)] *)
