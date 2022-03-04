open Definitions

(** [Params] wraps the types [S] is parameterized on *)
module type Params = sig
  type typ
  (** [typ] is the type of a value wrapped in a node *)

  val typ_equal : typ -> typ -> bool
  (** [typ_equal t1 t2] is [true] iff [t1] and [t2] represent equivalent
      types *)

  val mismatch : expect:typ -> typ -> TypeError.error
  (** [mismatch ~expect got] is an error representing a type mismatch
      between [t1] and [t2] *)

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

  val positioned :
    error:TypeError.error -> 'a t -> TypeError.Positioned.error
  (** [positioned ~error node] is an [error] occuring at position
      [position node]*)

  val assert_eq : expect:typ -> 'a t -> unit TypeError.Positioned.result
  (** [assert_eq ~expect v] is [Ok ()] if [expect] and the type of [v],
      [got] represent the same type and [Error (mismatch expect got)]
      otherwise. *)
end

(** [Expr] is a module wrapping an expression node *)
module Expr : sig
  include S with type typ = expr and type context = Context.context

  val mismatch_sub : expect:[< typ ] -> [< typ ] -> TypeError.error
  (** Same as [mismatch] but accepts subtypes of [typ] *)

  val assert_eq_sub :
    expect:[< typ ] -> 'a t -> unit TypeError.Positioned.result
  (** Same as [assert_eq] but accepts a subtype of [typ] *)

  val assert_int : 'a t -> unit TypeError.Positioned.result
  (** [assert_int expr] is [Ok ()] if [expr] has the int type and
      [Error Mismatch] otherwise *)

  val assert_bool : 'a t -> unit TypeError.Positioned.result
  (** [assert_bool expr] is [Ok ()] if [expr] has the boolean type and
      [Error Mismatch] otherwise *)
end

type 'a expr = 'a Expr.t
(** [expr] is the type of an expression node *)

(** [Stmt] is a module wrapping a statement node *)
module Stmt : sig
  include S with type typ = stmt and type context = Context.fn

  val assert_unit : 'a t -> unit TypeError.Positioned.result
  (** [assert_unit stmt] is [Ok ()] if [expr] has the unit type and
      [Error StmtMismatch] otherwise *)

  val make_unit : 'a -> ctx:context -> pos:Position.t -> 'a t
  (** [make_unit v ~ctx ~pos] is [make v ~ctx ~typ:`Unit ~pos] *)

  val make_void : 'a -> ctx:context -> pos:Position.t -> 'a t
  (** [make_void v ~ctx ~pos] is [make v ~ctx ~typ:`Void ~pos] *)
end

type 'a stmt = 'a Stmt.t
(** [stmt] is the type of a statement node *)