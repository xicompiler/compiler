open Core

include
  Abstract.S
    with module Expr.Node := Type.Node.Expr
     and module Stmt.Node := Type.Node.Stmt

(** [Error] represents a semantic error in the AST *)
module Error : module type of struct
  include Type.Error.Positioned
end

type expr_result = Expr.node Error.result
(** An [expr_result] is either [Ok expr] or [Error type_error] *)

type stmt_result = Stmt.node Error.result
(** An [stmt_result] is either [Ok stmt] or [Error type_error] *)

type nonrec result = t Error.result
(** A [result] is either [Ok ast] where [ast] is a decorated AST or
    [Error err] where [err] details the semantic error causing
    decoration to fail *)
