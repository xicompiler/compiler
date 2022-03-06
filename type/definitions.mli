type tau = Tau.t [@@deriving sexp_of]
(** [tau] is either a primitive or an array type. *)

type expr = Expr.t [@@deriving sexp_of]
(** [expr] is the type of an expression node in Xi. *)

type term = Term.t [@@deriving sexp_of]
(** [term] is a type in procedures, functions, and multiple assignments. *)

type stmt = Stmt.t [@@deriving sexp_of]
(** [stmt] is the type of the outcome of evaluating a statement. *)

type id = Id.t [@@deriving sexp_of]
(** [id] is a type used in an environment entry. *)
