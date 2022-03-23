open Subtype

type expr = expr Subtype.expr
(** An [expr] is a low-level intermediate representation expression *)

(** [Stmt] is the type of a statement in lowered IR *)
module Stmt : sig
  type base =
    [ expr Subtype.stmt
    | `Call of expr * expr list
    ]
  (** [base] represetnts a base statement in IR, excluding CJumps *)

  type t =
    [ base
    | expr cjump2
    ]
  (** A [t] is a low-level intermediate representation statement *)
end

val log_neg : expr -> expr
(** [log_neg expr] is the logical negation of [expr] *)

type stmt = Stmt.t
(** A [stmt] is a low-level intermediate representation statement *)

type t = stmt list
(** [t] is the representation of a program in lowered IR, a list of
    statement s*)
