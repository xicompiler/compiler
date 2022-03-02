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

  (** [env] is a type used in an environment entry. *)
  type env =
    | Var of tau
    | Ret of kind
    | Fn of kind * kind

  (** An [error] is the type of a Xi type error *)
  type error = Unbound

  type nonrec 'a result = ('a, error) result
  (** An ['a result] is either [Ok 'a] or [Error error] *)
end

module Context : Map.S with type Key.t = string
(** [Context] is a module representing a static typing context *)

type context = Type.env Context.t
(** [context] is the type of a static typing context *)

(** [ContextNode] represents a node containing a static context *)
module type ContextNode = sig
  include Node.S

  type typ
  (** [typ] is the type of a value wrapped in a node *)

  val context : 'a t -> context
  (** [context node] is the context of node *)

  val typ : 'a t -> typ
  (** [typ v] is the type of the value wrapped in [v] *)

  val make : 'a -> ctx:context -> typ:typ -> 'a t
  (** [make v ~ctx ~typ] is a node wrapping value [v] with context [ctx]
      and type [typ] *)
end

module Ex : ContextNode with type typ := Type.expr
(** [Ex] is a module wrapping an expression node *)

module St : ContextNode with type typ := Type.stmt
(** [St] is a module wrapping a statement node *)

include
  Abstract.S with module Expr.Node := Ex and module Stmt.Node := St

type nonrec result = t Type.result
(** A [result] is either [Ok ast] where [ast] is a decorated AST, or
    [Error terr] where [terr] is a type error *)
