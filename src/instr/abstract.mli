type t = Operand.Abstract.t Generic.t
(** [t] is the type of an abstract assembly instruction *)

val def : t -> Reg.Abstract.Set.t
(** [def instr] is the register written to by [instr], if any *)

module Asm : sig
  type t = Operand.Abstract.t Generic.Asm.t
  (** [asm] is the type of abstract assembly *)

  include Util.Stringable.S with type t := t
end

val munch : gensym:(unit -> string) -> Ir.Reorder.toplevel list -> Asm.t
(** [munch ~gensym top] is the global data and abstract assembly
    instructions having the same effect as [top] *)
