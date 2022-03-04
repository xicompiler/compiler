open! Core
include module type of Definitions
include module type of TypeError

type context = Context.context
(** [context] is the type of a static typing context *)

module Tau : module type of Tau
(** [Tau] is the type of a type expressible in Xi *)

module Context : module type of Context
(** [Context] is the type of a static typing context *)

val lub : stmt -> stmt -> stmt
(** [lub t1 t2] is [`Void] iff both of [t1] and [t2] are [`Void] and
    [`Unit] otherwise *)

val lub_stmt : 'a TypeNode.stmt -> 'a TypeNode.stmt -> stmt
(** [lub_stmt s1 s2] is [lub t1 t2] if [s1] has type [t1] and [s2] has
    type [t2] *)

val tau_of_expr : expr -> tau option
(** [tau_of_expr e] is [Some t] if [e] is tau type [t] and [None]
    otherwise *)

val assert_bool : 'a TypeNode.expr -> unit result
(** [assert_bool epr] is [Ok ()] if [expr] has the boolean type and
    [Error Mismatch] otherwise *)

val tau_of_expr_res : expr -> tau result
(** [tau_of_expr_res e] is [Ok t] if [e] is tau type [t] and
    [Error ExpectedTau] otherwise *)

val mismatch : [< expr ] -> [< expr ] -> error
(** [mismatch t1 t2] is [Mismatch (t1 :> expr, t2 :> expr)] *)

val equal_expr : [< expr ] -> [< expr ] -> bool
(** [equal_expr t1 t2] is [true] iff [t1] and [t2] represent equivalent
    expression types *)

val assert_eq : expr:'a TypeNode.expr -> [< expr ] -> unit result
(** [assert_eq ~expr t] is [Ok ()] if [got] and the type of [expr]
    represent the same type and [Error Mismatch] otherwise. *)

module Node : module type of TypeNode
(** [Node] is the type of a decorated AST node *)

module Error : module type of TypeError
(** [Error] is the type of a semantic error *)