(** [t] is the type of a generic instruction in x86 *)
type 'a t =
  | Jmp of 'jmp
  | Jcc of ConditionCode.t * Ir.label
  | Setcc of ConditionCode.t * 'reg8
  | Cmp of 'dest * 'operand
  | Test of 'dest * 'operand
  | Push of 'operand
  | Pop of 'dest
  | Mov of 'dest * 'operand
  | IMul of 'mul
  | Inc of 'dest
  | Dec of 'dest
  | Call of 'dest
  | IDiv of 'dest
  | Shl of 'dest * Imm.t
  | Shr of 'dest * Imm.t
  | Sar of 'dest * Imm.t
  | Add of 'dest * 'operand
  | Sub of 'dest * 'operand
  | Xor of 'dest * 'operand
  | And of 'dest * 'operand
  | Or of 'dest * 'operand
  | Lea of 'reg * 'reg Operand.Mem.generic
  | Ret
  constraint
    'a =
    < reg : 'reg
    ; reg8 : 'reg8
    ; dest : 'dest
    ; operand : 'operand
    ; jmp : 'jmp
    ; mul : 'mul >

val jnz : Ir.label -> 'a t
(** [jnz l] is [Jcc (Nz, l)] *)
