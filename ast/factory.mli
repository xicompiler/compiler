open Core

(** [Make (Ex) (St) (Tp)] is a concrete AST with each expression wrapped
    in a node of type [Ex.t] and each statement wrapped in a node of
    type [St.t] *)
module Make (Ex : Node.S) (St : Node.S) (Tp : Node.S) :
  Abstract.S
    with module Expr.Node = Ex
     and module Stmt.Node = St
     and module Toplevel.Node = Tp
