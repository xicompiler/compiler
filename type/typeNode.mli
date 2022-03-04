open Definitions
open TypeError

(** [Params] wraps the types [S] is parameterized on *)
module type Params = sig
  type typ
  (** [typ] is the type of a value wrapped in a node *)

  type context
  (** [context] is the type of the context wrapped in a node *)
end

(** [S] represents an abstract node *)
module type S = sig
  include Node.S
  include Params

  val context : 'a t -> context
  (** [context node] is the context of node *)

  val typ : 'a t -> typ
  (** [typ v] is the type of the value wrapped in [v] *)

  val make : 'a -> ctx:context -> typ:typ -> pos:Position.t -> 'a t
  (** [make v ~ctx ~typ] is a node wrapping value [v] with context [ctx]
      and type [typ] *)

  val position : 'a t -> Position.t
  (** [position node] is the position corresponding to [node] *)
end

(** [Expr] is a module wrapping an expression node *)
module Expr : S with type typ = expr and type context = Context.context

type 'a expr = 'a Expr.t
(** [expr] is the type of an expression node *)

(** [Stmt] is a module wrapping a statement node *)
module Stmt : sig
  include S with type typ = stmt and type context = Context.fn

  val make_unit : 'a -> ctx:context -> pos:Position.t -> 'a t
  (** [make_unit v ~ctx ~pos] is [make v ~ctx ~typ:`Unit ~pos] *)
end

type 'a stmt = 'a Stmt.t
(** [stmt] is the type of a statement node *)