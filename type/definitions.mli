type tau = Tau.t
(** [tau] is either a primitive or an array type. *)

type expr =
  [ tau
  | `Tuple of tau list
  ]
(** [expr] is the type of an expression node in Xi. *)

type term =
  [ expr
  | `Unit
  ]
(** [kind] is a type in procedures, functions, and multiple assignments. *)

type stmt =
  [ `Unit
  | `Void
  ]
(** [stmt] is the type of the outcome of evaluating a statement. *)

(** [id] is a type used in an environment entry. *)
type id =
  | Var of tau
  | Fn of term * term

val lub : stmt -> stmt -> stmt
(** [lub t1 t2] is [`Void] iff both of [t1] and [t2] are [`Void] and
    [`Unit] otherwise *)
