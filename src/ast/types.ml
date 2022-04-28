open Generic

module type Params = sig
  type expr
  type stmt
  type toplevel
end

module type S = sig
  module Data : Params

  type expr = Data.expr Expr.t
  type stmt = (Data.expr, Data.stmt) Stmt.t
  type source = (Data.expr, Data.stmt, Data.toplevel) Toplevel.Source.t
  type intf = Data.toplevel Toplevel.intf
  type t = (Data.expr, Data.stmt, Data.toplevel) Generic.t
end

module Make (Data : Params) = struct
  type expr = Data.expr Expr.t
  type stmt = (Data.expr, Data.stmt) Stmt.t
  type source = (Data.expr, Data.stmt, Data.toplevel) Toplevel.Source.t
  type intf = Data.toplevel Toplevel.intf
  type t = (Data.expr, Data.stmt, Data.toplevel) Generic.t
end
