open Core

(** [ContextNode] represents a node containing a static context *)
module type ContextNode = sig
  include Node.S

  type typ
  (** [typ] is the type of a value wrapped in a node *)

  type context
  (** [context] is the type of the context wrapped in a node *)

  val context : 'a t -> context
  (** [context node] is the context of node *)

  val typ : 'a t -> typ
  (** [typ v] is the type of the value wrapped in [v] *)

  val make : 'a -> ctx:context -> typ:typ -> 'a t
  (** [make v ~ctx ~typ] is a node wrapping value [v] with context [ctx]
      and type [typ] *)
end

(** [Ex] is a module wrapping an expression node *)
module Ex :
  ContextNode
    with type typ := Type.expr
     and type context := Type.context

(** [St] is a module wrapping a statement node *)
module St :
  ContextNode
    with type typ := Type.stmt
     and type context := Type.Context.fn

include
  Abstract.S with module Expr.Node := Ex and module Stmt.Node := St

type nonrec result = t Type.result
(** A [result] is either [Ok ast] where [ast] is a decorated AST, or
    [Error terr] where [terr] is a type error *)
