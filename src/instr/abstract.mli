type t = Operand.Abstract.t Generic.t [@@deriving sexp]
(** [t] is the type of an abstract assembly instruction *)

include Util.Stringable.S with type t := t

module Asm : sig
  type t = Operand.Abstract.t Generic.Asm.t
  (** [asm] is the type of abstract assembly *)

  include Util.Stringable.S with type t := t
end

val munch : gensym:(unit -> string) -> Ir.Reorder.toplevel list -> Asm.t
(** [munch ~gensym top] is the global data and abstract assembly
    instructions having the same effect as [top] *)

val def : t -> Reg.Abstract.Set.t
(** [def instr] is the set of all registers updated by [instr] *)

val use : t -> Reg.Abstract.Set.t
(** [use instr] is the set of all registers used by [instr] *)

val regs : t -> Reg.Abstract.Set.t
(** [regs instr] is the set of all registers in [instr] *)

val map : t -> f:(Reg.Abstract.t -> Reg.Abstract.t) -> t
(** [map instr ~f] applies [f] to the operands of [instr] and returns
    the result*)

val map_list : f:(Reg.Abstract.t -> Reg.Abstract.t) -> t list -> t list
(** [map_list ~f instrs] applies [f] to every instruction in [instrs] *)

val map_concrete_list :
  f:(Reg.Abstract.t -> Reg.t) -> t list -> Concrete.t list
(** [map_concrete_list ~f instrs] applies concretizing function [f] to
    every instruction in [instrs] *)
