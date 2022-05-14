type t = Operand.Abstract.t Generic.t
(** [t] is the type of an abstract assembly instruction *)

module Asm : sig
  type t = Operand.Abstract.t Generic.Asm.t
  (** [asm] is the type of abstract assembly *)

  include Util.Stringable.S with type t := t
end

val munch : gensym:(unit -> string) -> Ir.Reorder.toplevel list -> Asm.t
(** [munch ~gensym top] is the global data and abstract assembly
    instructions having the same effect as [top] *)

val def : t -> Reg.Abstract.Set.t
(** [def instr] is the set of all operands updated by [instr] *)

val use : t -> Reg.Abstract.Set.t
(** [use instr] is the set of all operands used by [instr] *)

val map :
  t ->
  f:(Reg.Abstract.t -> ([> 'a Operand.generic ] as 'a)) ->
  'a Generic.t
(** [map instr ~f] applies [f] to every operand within [instr] *)
