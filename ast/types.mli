open Generic

(** [Params] represent the types needed to construct type information
    for a concrete AST *)
module type Params = sig
  type expr
  (** [expr] is the type of data carried by expression nodes *)

  type stmt
  (** [stmt] is the type of data carried by statement nodes *)

  type toplevel
  (** [toplevel] is the type of data carried by toplevel nodes *)
end

(** [S] represents the types present in a concrete AST *)
module type S = sig
  module Data : Params
  (** [Data] represents the types of data carried by an expression,
      statement, and toplevel definition in an ASt *)

  type expr = Data.expr Expr.t
  (** [expr] is an expression node in a concrete AST *)

  type stmt = (Data.expr, Data.stmt) Stmt.t
  (** [stmt] is an statement node in a concrete AST *)

  type source = (Data.expr, Data.stmt, Data.toplevel) Toplevel.Source.t
  (** [source] is an toplevel node in a concrete AST *)

  type intf = Data.toplevel Toplevel.intf
  (** [intf] is an interface node in a concrete AST *)

  type t = (Data.expr, Data.stmt, Data.toplevel) Generic.t
  (** [t] is the type of a concrete AST *)
end

(** [Make (Data)] is a concrete AST parameterized over data types in
    [Data] *)
module Make (Data : Params) : S with module Data := Data
