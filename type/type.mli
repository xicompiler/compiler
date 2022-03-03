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

val tau_of_expr : expr -> tau option
(** [tau_of_expr e] is [Some t] if [e] is tau type [t] and [None]
    otherwise *)

val assert_bool : expr -> unit result
(** [assert_bool e] is [Ok ()] if [e] is the boolean type and
    [Error Mismatch] otherwise *)

val tau_of_expr_res : expr -> tau result
(** [tau_of_expr_res e] is [Ok t] if [e] is tau type [t] and
    [Error ExpectedTau] otherwise *)

val mismatch : [< expr ] -> [< expr ] -> error
(** [mismatch t1 t2] is [Mismatch (t1 :> expr, t2 :> expr)] *)

val assert_eq : exp:[< expr ] -> [< expr ] -> unit result
(** [assert_eq got ~exp] is [Ok ()] if [got] and [exp] represent the
    same type and [Error Mismatch] otherwise. *)

module Node : module type of TypeNode
(** [Node] is the type of a decorated AST node *)