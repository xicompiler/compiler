open Error
include module type of Definitions

val assert_unit : [< term ] -> unit result
(** [assert_unit term] is [Ok ()] if [term] is a unit type and
    [Error ExpectedUnit] otherwise *)

val assert_void : [< stmt ] -> unit result
(** [assert_unit stmt] is [Ok ()] if [stmt] is a void type and
    [Error ExpectedUnit] otherwise *)

module Error : module type of Error
(** [Error] is the type of a semantic error *)
