open Core
include module type of Definitions
include module type of TypeError
include module type of Conversions

module Tau : module type of Tau
(** [Tau] is the type of a type expressible in Xi *)

val assert_array : [< expr ] -> unit result
(** [assert_array expr] is [Ok ()] if [expr] is an array type and
    [Error Mismatch] otherwise *)

val assert_unit : [< term ] -> unit result
(** [assert_unit term] is [Ok ()] if [term] is a unit type and
    [Error ExpectedUnit] otherwise *)

val assert_void : [< stmt ] -> unit result
(** [assert_unit stmt] is [Ok ()] if [stmt] is a void type and
    [Error ExpectedUnit] otherwise *)

module Error : module type of TypeError
(** [Error] is the type of a semantic error *)
