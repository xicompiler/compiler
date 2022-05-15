open Type.Error

type id = string Position.entry
(** [id] represents the type of an identifier with an associated
    position *)

val expected_tau : id -> Positioned.t
(** [expected_tau id] is an [Positioned.t] indicating that [id] is
    expected to have type [tau] *)

val expected_fn : id -> Positioned.t
(** [expected_fn id] is an [Positioned.t] indicating that [id] is
    expected to have type *)

val expected_record : id -> Positioned.t
(** [expected_record id] is an [Positioned.t] indicating that [id] is
    expected to have type record *)

val bound : id -> Positioned.t
(** [bound id] is an [Positioned.t] indicating that [id] is already
    bound in the context *)

val unbound : id -> Positioned.t
(** [unbound id] is an [Positioned.t] indicating that [id] is not bound
    in the context *)

val fn_mismatch : id -> Positioned.t
(** [fn_mismatch id] is an [Positioned.t] indicating that that
    declaration and implementation of function named [id] do not match *)

val record_mismatch : id -> Positioned.t
(** [record_mismatch id] is an [Positioned.t] indicating that that
    declaration and implementation of record named [id] do not match *)

val unbound_intf : id -> Positioned.t
(** [unbound_intf id] is an [Positioned.t] indicating that interface
    with identifier [id] is unbound *)
