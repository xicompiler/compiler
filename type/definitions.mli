module Tau : module type of Tau
(** [Tau] represents a type expressible in the Xi language *)

type tau = Tau.t [@@deriving sexp_of]
(** [tau] is either a primitive or an array type. *)

module Expr : module type of Expr
(** [Expr] represents the type an expression can have*)

type expr = Expr.t [@@deriving sexp_of]
(** [expr] is the type of an expression node in Xi. *)

module Term : module type of Term
(** [Term] represents the type a term in Xi can have *)

type term = Term.t [@@deriving sexp_of]
(** [term] is a type in procedures, functions, and multiple assignments. *)

module Stmt : module type of Stmt
(** [Stmt] represents the type a statement in Xi can have*)

type stmt = Stmt.t [@@deriving sexp_of]
(** [stmt] is the type of the outcome of evaluating a statement. *)

module FnType : module type of FnType
(** [FnType] represents a function type in [Xi] *)

module Bound : module type of Bound
(** [Bound] represents the type an identifier bound in the context can
    have *)

type bound = Bound.t [@@deriving sexp_of]
(** [bound] is a type used in an environment entry. *)
