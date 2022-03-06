open Sexplib.Std

type tau = Tau.t [@@deriving sexp_of]
type expr = Expr.t [@@deriving sexp_of]
type term = Term.t [@@deriving sexp_of]
type stmt = Stmt.t [@@deriving sexp_of]
type id = Id.t [@@deriving sexp_of]
