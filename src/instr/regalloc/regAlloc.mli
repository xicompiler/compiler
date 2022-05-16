val allocate :
  opt:Opt.t ->
  gensym:(unit -> string) ->
  Abstract.Asm.t ->
  Concrete.Asm.t
(** [allocate ~opt asm] is the concretized version of [asm] *)

module Color : module type of struct
  include Color
end
