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

val map : f:(Reg.Abstract.t -> Reg.Abstract.t) -> t list -> t list
(** [map ~f instrs] applies [f] to every instruction in [instrs] *)

val map_concrete :
  f:(Reg.Abstract.t -> Reg.t) -> t list -> Concrete.t list
(** [map_concrete ~f instrs] applies concretizing function [f] to every
    instruction in [instrs] *)
