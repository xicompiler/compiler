open Core
open Subtype

type stmt =
  [ Lir.Stmt.base
  | `CJump of Lir.expr * label
  ]
(** [stmt] is the type of a reordered statement *)

type t = stmt list
(** [t] is the type representing a reordered program, a sequence of
    statements *)

val reorder : Lir.t -> gensym:(unit -> label) -> t
(** [reorder ir ~gensym] is the reordered list of statements
    corresponding to [ir], where fresh labels are generated using
    [gensym] *)
