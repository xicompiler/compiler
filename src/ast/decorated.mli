open Core

(** [Data] is the data used to decorate a node in the decorated AST *)
module Data : sig
  (** [S] is the base type of Decorated data *)
  module type S = sig
    type t
    (** [t] is the base type of decorated data in the AST *)

    val context : t -> Context.t
    (** [context node] is the context of node *)

    val position : t -> Position.t
    (** [position node] is the position corresponding to [node] *)
  end

  (** [Term] represents an abstract node *)
  module type Term = sig
    include S

    type typ
    (** [typ] is the type of a value wrapped in a node *)

    val typ : t -> typ
    (** [typ v] is the type of the value wrapped in [v] *)
  end

  (** [Toplevel] represents a node at the Toplevel *)
  module Toplevel : sig
    include S

    val create : ctx:Context.t -> pos:Position.t -> t
    (** [create ~ctx ~pos] is toplevel data wrapping context [ctx] and
        position [pos] *)
  end

  type toplevel = Toplevel.t
  (** [toplevel] is an alias for [Toplevel.t] *)

  (** [Expr] is a module wrapping an expression node *)
  module Expr : sig
    include Term with type typ = Type.expr

    val assert_eq :
      expect:[< typ ] -> t -> unit Type.Error.Positioned.result
    (** [assert_eq ~expect v] is [Ok ()] if [expect] and the type of
        [v], [got] represent the same type and
        [Error (mismatch expect got)] otherwise. *)

    val create : typ:[< typ ] -> ctx:Context.t -> pos:Position.t -> t
    (** [create ~ctx ~typ ~pos] is data wrapping context [ctx] and type
        [typ] *)

    val create_int : ctx:Context.t -> pos:Position.t -> t
    (** [create_int ~ctx ~pos] is an AST decoration with type [`Int],
        context [ctx], and position [pos] *)

    val create_bool : ctx:Context.t -> pos:Position.t -> t
    (** [create_bool ~ctx ~pos] is an AST decoration type [`Bool],
        context [ctx], and position [pos] *)

    val join :
      t -> expect:Type.tau -> Type.Tau.t Type.Error.Positioned.result
    (** [join exp ~expect] is [Ok t], where [t] is [join t1 expect] and
        [t1] is the type of [t1], [Error ExpectedTau] if [t1] is not of
        type [Type.tau], or [Error (ExprMismatch (expect, t1))] if
        [expect] and [t1] have no common supertype. *)

    val assert_int : t -> unit Type.Error.Positioned.result
    (** [assert_int expr] is [Ok ()] if [expr] has the int type and
        [Error ExprMismatch] otherwise *)

    val assert_bool : t -> unit Type.Error.Positioned.result
    (** [assert_bool expr] is [Ok ()] if [expr] has the boolean type and
        [Error ExprMismatch] otherwise *)

    val assert_array : t -> unit Type.Error.Positioned.result
    (** [assert_array e] is [Ok ()] if [e] is an array type and
        [Error ExpectedArray] otherwise *)

    val to_tau : t -> Type.Tau.t Type.Error.Positioned.result
    (** [to_tau exp] is [Ok t] where [t] is the tau type wrapped in
        [exp], or [Error ExpectedTau] if the type wrapped by [t] is a
        Tuple. *)
  end

  type expr = Expr.t
  (** [expr] is an alias for [Expr.t] *)

  (** [Stmt] is a module wrapping a statement node *)
  module Stmt : sig
    include Term with type typ = Type.stmt

    val create : typ:typ -> ctx:Context.t -> pos:Position.t -> t
    (** [create ~ctx ~typ ~pos] is data wrapping context [ctx] and type
        [typ] *)

    val assert_unit : t -> unit Type.Error.Positioned.result
    (** [assert_unit stmt] is [Ok ()] if [stmt] has the unit type and
        [Error StmtMismatch] otherwise *)

    val assert_void : t -> unit Type.Error.Positioned.result
    (** [assert_void stmt] is [Ok ()] if [stmt] has the unit type and
        [Error StmtMismatch] otherwise *)

    val create_unit : ctx:Context.t -> pos:Position.t -> t
    (** [create_unit ~ctx ~pos] is [create v ~ctx ~typ:`Unit ~pos] *)

    val create_void : ctx:Context.t -> pos:Position.t -> t
    (** [create_void ~ctx ~pos] is [create v ~ctx ~typ:`Void ~pos] *)

    val lub : t -> t -> typ
    (** [lub s1 s2] is [Definitions.lub t1 t2] if [s1] has type [t1] and
        [s2] has type [t2] *)
  end

  type stmt = Stmt.t
  (** [stmt] is an alias for [Stmt.t] *)
end

include Types.S with module Data := Data

type result = t Type.Error.Positioned.result
(** A [result] is either [Ok ast] where [ast] is a decorated [ast], or
    [Error err], where [err] is a type error *)
