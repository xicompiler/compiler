val allocate : opt:Opt.t -> Abstract.Asm.t -> Concrete.Asm.t
(** [allocate ~opt asm] is the concretized version of [asm] *)

module Color : module type of struct
  include Color
end
