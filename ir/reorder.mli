open Core
open Subtype

type stmt =
  [ Lir.expr Subtype.stmt
  | `CJump of Lir.expr * label
  ]
(** [stmt] is the type of a reordered statement *)

type toplevel =
  [ `Func of label * stmt list
  | `Data of label * int64 list
  ]

type t = toplevel list
(** [t] is the type representing a reordered program, a sequence of
    statements *)

val reorder : gensym:(unit -> label) -> Lir.t -> t
(** [reorder ir ~gensym] is the reordered list of statements
    corresponding to [ir], where fresh labels are generated using
    [gensym] *)
