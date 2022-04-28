open Core
open Subtype

type t
(** [t] is the type of a basic block *)

val of_lir : Lir.stmt list -> t list
(** [of_lir lir] is a sequence of basic blocks representing [lir] *)

val first : t -> Lir.stmt option
(** [first block] is the first statement of basic block [block], if
    present *)

val set_first : t -> stmt:Lir.stmt -> t
(** [set_first block s] is [block] with its first statement set to [s] *)

val insert_first : t -> stmt:Lir.stmt -> t
(** [insert_first block s] is [block] with [s] pushed onto the front *)

val remove_first : t -> t
(** [remove_first block] is [block] with its first statement removed, if
    present *)

val last : t -> Lir.stmt option
(** [last block] is the last statement of basic block [block] *)

val set_last : t -> stmt:Lir.stmt -> t
(** [set_last block s] is [block] with its last statement set to [s] *)

val insert_last : t -> stmt:Lir.stmt -> t
(** [insert_last block s] is [block] with [s] pushed onto the back *)

val remove_last : t -> t
(** [remove_last block] is [block] with its last statement removed, if
    present *)

val label : t -> label option
(** [label t] is the label that [t] begins with if present, or [None] if
    not present *)

val has_label : t -> label:label -> bool
(** [has_label block ~label] is [true] iff [block] begins with label
    [label] *)

val insert_label : t -> label:label -> t
(** [insert_label t ~label] is [t] with [`Label label] pushed onto the
    front *)

val to_list : t -> Lir.stmt list
(** [to_list block] is the representation of [block] as a list of Lir
    statements *)

include Util.Stringable.S with type t := t
