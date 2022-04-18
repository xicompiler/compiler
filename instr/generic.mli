(** [t] is the type of a generic instruction in x86 *)
type 'a t =
  | Label of Ir.label
  | Enter of Imm.t * Imm.t
  | Jmp of 'a
  | Jcc of ConditionCode.t * Ir.label
  | Setcc of ConditionCode.t * 'a
  | Cmp of 'a * 'a
  | Test of 'a * 'a
  | Push of 'a
  | Pop of 'a
  | IMul of [ `M of 'a | `RM of 'a * 'a | `RMI of 'a * 'a * Imm.t ]
  | Inc of 'a
  | Dec of 'a
  | Call of 'a
  | IDiv of 'a
  | Shl of 'a * Imm.t
  | Shr of 'a * Imm.t
  | Sar of 'a * Imm.t
  | Add of 'a * 'a
  | Sub of 'a * 'a
  | Xor of 'a * 'a
  | And of 'a * 'a
  | Or of 'a * 'a
  | Lea of 'a * 'a
  | Mov of 'a * 'a
  | Movzx of 'a * 'a
  | Leave
  | Ret
[@@deriving variants]

val jnz : Ir.label -> 'a t
(** [jnz l] is [Jcc (Nz, l)] *)

val zero : 'a -> 'a t
(** [zero e] is [Xor (e, e)] *)

type 'a instr = 'a t
(** [instr] is alias for [t] *)

(** [Asm] represents assembly *)
module Asm : sig
  (** [Data] represents static data in the data segment *)
  module Data : sig
    type t
    (** [t] is the type of static data in the data segment *)

    val label : t -> Ir.label
    (** [label data] is the name of static data [data] *)

    val value : t -> int64 list
    (** [value data] is the value of the static data [data]. It is a
        singleton list if [data] only stores one value *)

    val create : label:Ir.label -> value:int64 list -> t
    (** [create ~label ~value] is static data with name [label] and
        value [value] *)
  end

  (** [Fn] represents a function in assembly; a name together with a
      sequence of instructions representing the body *)
  module Fn : sig
    type 'a t
    (** ['a t] is a function with instruction operand types ['a] *)

    type 'a body = 'a instr list
    (** ['a body] is the type of a function body with instruction
        operand type ['a] *)

    val name : 'a t -> string
    (** [name fn] is the name of function [fn] *)

    val body : 'a t -> 'a body
    (** [body fn] is the list of instructions comprising the body of
        [fn] *)

    val map_body : f:('a body -> 'b body) -> 'a t -> 'b t
    (** [map_body ~f fn] is [fn] with its name unchanged and body set to
        [~f fn.body] *)

    val create : name:string -> body:'a body -> 'a t
    (** [create ~name ~body] is an assembly function with name [name]
        and body [body] *)
  end

  (** [Directive] represents an x86 directive *)
  module Directive : sig
    (** ['a t] is the type of an x86 directive with operand type ['a] *)
    type 'a t =
      | Text of 'a Fn.t list
      | Data of Data.t list
      | Globl of Ir.label list
      | IntelSyntax of [ `prefix | `noprefix ]
  end

  type 'a t = 'a Directive.t list
  (** ['a t] is the representation of an assebly program as list of x86
      directives *)

  val to_lines : f:('a -> string) -> 'a t -> string list
  (** [to_lines ~f asm] is the representation of concrete assembly [asm]
      as a list of lines *)

  val to_string : f:('a -> string) -> 'a t -> string
  (** [to_string ~f asm] is the string representation of concrete
      assembly [asm] *)
end

val to_string : f:('a -> string) -> 'a t -> string
(** [to_string ~f instr] is the string representation of concrete
    [instr] *)
