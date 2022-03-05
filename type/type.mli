open Core
include module type of Definitions
include module type of TypeError
include module type of Conversions

type context = Context.t
(** [context] is the type of a static typing context *)

module Tau : module type of Tau
(** [Tau] is the type of a type expressible in Xi *)

module Context : module type of Context
(** [Context] is the type of a static typing context *)

val assert_array : [< expr ] -> unit result
(** [assert_array expr] is [Ok ()] if [expr] is an array type and
    [Error Mismatch] otherwise *)

val assert_unit : [< term ] -> unit result
(** [assert_unit term] is [Ok ()] if [term] is a unit type and
    [Error ExpectedUnit] otherwise *)

module Node : module type of TypeNode
(** [Node] is the type of a decorated AST node *)

module Error : module type of TypeError
(** [Error] is the type of a semantic error *)
