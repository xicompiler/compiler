val concretize_reg : shuttle:Shuttle.t -> Reg.Abstract.t -> Reg.t
(** [concretize_reg ~shuttle reg] concretizes abstract register [reg] *)

val concretize_instr :
  shuttle:Shuttle.t ->
  spill:(Reg.Abstract.t -> bool) ->
  Abstract.t ->
  Concrete.t
(** [concretize_instr ~shuttle ~spill instr] concretizes abstract
    instruction [instr] *)
