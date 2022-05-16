type t = Operand.t Generic.t

module Asm : sig
  type t = Operand.t Generic.Asm.t
  (** [asm] is the type of abstract assembly *)

  include Util.Stringable.S with type t := t
end

val to_string : t -> string
(** [to_string instr] is the string representation of concrete
    instruction [instr] *)
