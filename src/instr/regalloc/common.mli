val load :
  offset:int Reg.Abstract.Table.t ->
  src:Reg.Abstract.t ->
  [> Reg.Bit64.t ] ->
  Concrete.t
(** [load ~offset ~src dst] is a load instruction moving the contents of
    [src] to [dst] *)

val store :
  offset:int Reg.Abstract.Table.t ->
  dst:Reg.Abstract.t ->
  [> Reg.Bit64.t ] ->
  Concrete.t
(** [load ~offset ~dst src] is a store instruction moving the contents
    of [src] to [dst] *)

val rev_allocate_load :
  offset:int Reg.Abstract.Table.t ->
  shuttle:Shuttle.t ->
  init:Concrete.t list ->
  Reg.Abstract.t list ->
  Reg.Abstract.Set.t ->
  Abstract.t ->
  Concrete.t list * Shuttle.t
(** [rev_allocate_load ~offset ~shuttle ~init spills defs instr] loads
    [spills instr] into the appropriate shuttling registers *)

val rev_allocate_store :
  offset:int Reg.Abstract.Table.t ->
  shuttle:Shuttle.t ->
  init:Concrete.t list ->
  (Reg.Abstract.t -> bool) ->
  Reg.Abstract.Set.t ->
  Concrete.t list
(** [rev_allocate_store ~offset ~shuttle ~init spill defs] stores the
    value of the shuttling registers of [def] into the memory addresses
    if [spill def] is [true], for each element in [defs] *)