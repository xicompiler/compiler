open Core

(** [Type] is a module representing the Xi type system. *)
module Type : sig
  type tau = Tau.t
  (** [tau] is either a primitive or an array type. *)

  type expr =
    [ tau
    | `Tuple of tau list
    ]
  (** [expr] is the type of an expression node in Xi. *)

  type kind =
    [ expr
    | `Unit
    ]
  (** [kind] is a type in procedures, functions, and multiple
      assignments. *)

  type stmt =
    [ `Unit
    | `Void
    ]
  (** [stmt] is the type of the outcome of evaluating a statement. *)

  type env =
    [ `Var of tau
    | `Ret of kind
    | `Fn of kind * kind
    ]
  (** [env] is a type used in an environment entry. *)
end

module Context : Map.S with type Key.t = string
(** [Context] is a module representing a static typing context *)

type context = Type.env Context.t
(** [context] is the type of a static typing context *)

(** [ContextNode] represents a node containing a static context *)
module type ContextNode = sig
  include Node.S

  val context : 'a t -> context
  (** [context node] is the context of node *)
end

(** [Ex] is a module wrapping an expression node *)
module Ex : sig
  include ContextNode

  val typ : 'a t -> Type.expr
  (** [typ node] is the type of [node] *)
end

(** [St] is a module wrapping a statement node *)
module St : sig
  include ContextNode

  val typ : 'a t -> Type.stmt
  (** [typ node] is the type of [node] *)
end

include
  Abstract.S with module Expr.Node := Ex and module Stmt.Node := St
