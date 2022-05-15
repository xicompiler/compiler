val allocate : Abstract.Asm.t -> Concrete.Asm.t
(** [allocate asm] is the concretized version of [asm] *)

module Color : module type of struct
  include Color
end
