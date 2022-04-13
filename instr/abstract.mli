open Operand

type jmp =
  [ Dest.abstract
  | Ir.name
  ]
(** [jmp] is the type of an operand to an abstract [jmp] instruction *)

type mul =
  [ Dest.abstract
  | (Reg.abstract, Dest.abstract) Encoding.rm
  | (Reg.abstract, Dest.abstract) Encoding.rmi
  ]
(** [mul] is the type of an operand to an abstract [mul] or [imul]
    instruction *)

type t =
  < reg : Reg.abstract
  ; reg8 : Reg.Bit8.abstract
  ; dest : Dest.abstract
  ; operand : abstract
  ; jmp : jmp
  ; mul : mul >
  Generic.t
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

(** [Expr] contains functions for manipulating IR expressions and
    translating them into abstract assembly *)
module Stmt : sig
  type translation = t list
  (** A [translation] is a list of abstract assembly instruction *)

  val munch : gensym:(unit -> string) -> Ir.Reorder.stmt -> translation
  (** [munch ~gensym s] is the sequence of abstract assembly
      instructions having the same effect as [s] *)
end