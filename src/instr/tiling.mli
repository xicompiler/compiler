open Core

type 'a t
(** [t] is the type of a tile tiling an IR expression *)

val create :
  asm:'a Generic.t list -> ?cost:int -> dfs:int -> string -> 'a t
(** [create ~asm ~cost t] is a tiling with sequence of instructions
    [asm] and tiling cost [cost] that stores the result of its
    translation in temporary [t]. If not provided [asm] is the empty
    list [dfs] denotes the dfs numbering of the tiling *)

val add_instr : 'a Generic.t -> 'a t -> 'a t
(** [add_instr instr tiling] adds [instr] to the list of instrs of
    [tiling] *)

val asm : 'a t -> 'a Generic.t list
(** [asm tiling] is the list of instructions of tiling [tiling] in
    reverse order. O(1) *)

val cost : 'a t -> int
(** [cost tiling] is the cost of tiling [tiling] *)

val temp : 'a t -> string
(** [temp tiling] is the temporary that stores the result of the
    computation of [tiling] *)

val dfs : 'a t -> int
(** [dfs] is the dfs number of the tiling *)
