open Core
open Abstract

(** [Make (Types)] is a concrete AST with the expression types of
    [Types] *)
module Make (Types : Types.S) (Ex : Node.S) (St : Node.S) :
  S
    with module Expr.Node = Ex
     and module Stmt.Node = St
     and type integer := Types.integer
     and type boolean := Types.boolean
     and type 'a vector := 'a Types.vector
