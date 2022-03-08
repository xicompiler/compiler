open Type.Error.Positioned

type id = string Node.Position.t
(** [id] represents the type of an identifier with an associated
    position *)

val expected_tau : id -> error
(** [expected_tau id] is an [error] indicating that [id] is expected to
    have type [tau] *)

val expected_fn : id -> error
(** [expected_fn id] is an [error] indicating that [id] is expected to
    have type *)

val bound : id -> error
(** [bound id] is an [error] indicating that [id] is already bound in
    the context *)

val unbound : id -> error
(** [unbound id] is an [error] indicating that [id] is not bound in the
    context *)

val fn_mismatch : id -> error
(** [fn_mismatch id] is an [error] indicating that that declaration and
    implementation of function named [id] do not match *)

val unbound_intf : id -> error
(** [unbound_intf id] is an [error] indicating that interface with
    identifier [id] is unbound *)
