type t = Operand.Abstract.t Generic.t
(** [t] is the type of an abstract assembly instruction *)

(** [Expr] contains functions for manipulating IR expressions and
    translating them into abstract assembly *)
module Expr : sig
  type translation = t list * string
  (** A [translation] is a pair [(s, t)] where [s] is a sequence of
      abstract assembly instructions necessary to effect the movement of
      the translation of an expression into temporary [t] *)

  val munch : gensym:(unit -> string) -> Ir.Lir.expr -> translation
  (** [munch ~gensym e] is a pair [(s, t)] where [s] is a sequence of
      abstract assembly instructions necessary to effect the movement of
      the translation of [e] into temporary [t] *)
end

(** [Stmt] contains functions for manipulating IR statements and
    translating them into abstract assembly *)
module Stmt : sig
  type translation = t list
  (** A [translation] is a list of abstract assembly instruction *)

  val munch : gensym:(unit -> string) -> Ir.Reorder.stmt -> translation
  (** [munch ~gensym s] is the sequence of abstract assembly
      instructions having the same effect as [s] *)
end

module Asm : sig
  type t = Operand.Abstract.t Generic.Asm.t
  (** [asm] is the type of abstract assembly *)

  include Util.Stringable.S with type t := t
end

val munch : gensym:(unit -> string) -> Ir.Reorder.toplevel list -> Asm.t
(** [munch ~gensym top] is the global data and abstract assembly
    instructions having the same effect as [top] *)
