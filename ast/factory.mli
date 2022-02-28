open Core

(** [Make (Ex) (St)] is a concrete AST with each expression wrapped in a
    node of type [Ex.t] and each statement wrapped in a node of type
    [St.t] *)
module Make (Ex : Node.S) (St : Node.S) :
  Abstract.S with module Expr.Node = Ex and module Stmt.Node = St
