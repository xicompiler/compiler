open Core
open Generic
open Asm.Directive
open Util.Fn

(** [address_of ~offset t] is the memory operand of temp [t] in stack
    space, depending on [alloc] *)
let address_of ~offset t =
  let length = Hashtbl.length offset in
  let default () = succ length in
  let off = Hashtbl.find_or_add offset t ~default in
  let off = Int64.of_int (~-8 * off) in
  Mem.create ~offset:off `rbp

(** [choose_reg ?reg regs] chooses a register from [regs] that does not
    conflict with [reg] *)
let rec choose_reg ?reg regs =
  let ((r, tl) as split) = Util.List.pop_exn regs in
  match reg with
  | Some (#Reg.t as reg') when Reg.equal reg' r ->
      tl |> choose_reg ?reg |> Tuple2.map_snd ~f:(List.cons r)
  | Some _ | None -> split

(** [convert_temp ~offset ~init ~regs ?conflict t] is [s, mem, tl] where
    [s] is the reversed sequence of instructions to move [t] to a
    register in [t], followed by [init], and [tl] is the remaining
    avilable registers *)
let convert_temp ~offset ~init ~regs ?conflict t =
  let addr = address_of ~offset t in
  let reg, tl = choose_reg ?reg:conflict regs in
  (Mov ((reg :> Operand.t), `Mem addr) :: init, reg, tl)

let convert_reg ~offset ~init ~regs ?conflict = function
  | #Reg.t as r -> (init, r, regs)
  | #Ir.Temp.Virtual.t as t ->
      convert_temp ~offset ~init ~regs ?conflict t

let convert_index ~offset ~init ~regs = function
  | None -> (init, None, regs)
  | Some scaled ->
      let index = Mem.Index.index scaled in
      let init, index, regs = convert_reg ~offset ~init ~regs index in
      let index = Mem.Index.with_index ~index scaled in
      (init, Some index, regs)

let convert_mem ~offset ~init ~regs mem =
  let base = Mem.base mem in
  let init, base, regs = convert_reg ~offset ~init ~regs base in
  let f index = `Mem (Mem.with_registers ?index ~base mem) in
  mem |> Mem.index
  |> convert_index ~offset ~init ~regs
  |> Tuple3.map_snd ~f

let convert_operand ~offset ~init ~regs ?conflict = function
  | (`Name _ | `Imm _) as concrete -> (init, concrete, regs)
  | `Mem mem -> convert_mem ~offset ~init ~regs mem
  | #Reg.Abstract.t as t ->
      (convert_reg ~offset ~init ~regs ?conflict t
        :> Concrete.t list * Operand.t * Reg.t list)

let regs = [ `r8; `r9; `r10; `r11; `rsi; `rdi; `rax; `rcx; `rdx ]

let concretize ~offset ~init ~f op =
  let s, op, _ = convert_operand ~offset ~init ~regs op in
  f op :: s

let concretize2 ~offset ~init ~f ?conflict op1 op2 =
  let s1, op1, regs =
    convert_operand ~offset ~init ~regs ~conflict:op2 op1
  in
  let s2, op2, _ =
    convert_operand ~offset ~init:s1 ~regs ?conflict op2
  in
  f op1 op2 :: s2

let write_back ~offset ~init ~src t =
  let mem = `Mem (address_of ~offset t) in
  Mov (mem, src) :: init

let concretize2_rw ~offset ~init ~f op1 op2 =
  let init = concretize2 ~offset ~init ~f op1 op2 in
  match op1 with
  | #Ir.Temp.Virtual.t as t -> write_back ~offset ~init ~src:`r8 t
  | _ -> init

let concretize2_write ~offset ~init ~f e1 e2 =
  match e1 with
  | #Ir.Temp.Virtual.t as t ->
      let s, op, _ = convert_operand ~offset ~init ~regs e2 in
      let dst, regs = choose_reg ~reg:op regs in
      let init = f (dst :> Operand.t) op :: s in
      write_back ~offset ~init ~src:(dst :> Operand.t) t
  | _ -> concretize2_rw ~offset ~init ~f e1 e2

let concretize_setcc ~offset ~init cc = function
  | `Temp _ as t ->
      let init = Movzx (`r8, `r8b) :: Setcc (cc, `r8b) :: init in
      write_back ~offset ~init ~src:`r8 t
  | #Reg.Bit8.t as r ->
      Movzx (Reg.Bit8.to_64_bit r, r) :: Setcc (cc, r) :: init
  | #Operand.Abstract.t -> failwith "unexpected operand for setcc"

let concretize_pop ~offset ~init e =
  match e with
  | #Ir.Temp.Virtual.t as t ->
      let dst, regs = choose_reg regs in
      let init = pop (dst :> Operand.t) :: init in
      write_back ~offset ~init ~src:(dst :> Operand.t) t
  | _ -> concretize ~offset ~init ~f:pop e

let concretize_mul ~offset ~init = function
  | `M op -> concretize ~offset ~init ~f:(fun x -> IMul (`M x)) op
  | `RM (op1, op2) ->
      let f x y = IMul (`RM (x, y)) in
      concretize2_rw ~offset ~init ~f op1 op2
  | `RMI (op1, op2, imm) ->
      let f x y = IMul (`RMI (x, y, imm)) in
      concretize2_rw ~offset ~init ~f op1 op2

let rev_reg_alloc_instr ~offset init : Abstract.t -> Concrete.t list =
  function
  | (Label _ | Enter _ | Jcc _ | Leave | Ret) as instr -> instr :: init
  | Jmp op -> concretize ~offset ~init ~f:jmp op
  | Setcc (cc, reg8) -> concretize_setcc ~offset ~init cc reg8
  | Cmp (op1, op2) -> concretize2 ~offset ~init ~f:cmp op1 op2
  | Test (op1, op2) -> concretize2 ~offset ~init ~f:test op1 op2
  | Push op -> concretize ~offset ~init ~f:push op
  | Pop op -> concretize_pop ~offset ~init op
  | IMul mul -> concretize_mul ~offset ~init mul
  | Inc op -> concretize ~offset ~init ~f:inc op
  | Dec op -> concretize ~offset ~init ~f:dec op
  | Call op -> concretize ~offset ~init ~f:call op
  | IDiv op -> concretize ~offset ~init ~f:idiv op
  | Shl (op, imm) -> concretize ~offset ~init ~f:(fun x -> shl x imm) op
  | Shr (op, imm) -> concretize ~offset ~init ~f:(fun x -> shr x imm) op
  | Sar (op, imm) -> concretize ~offset ~init ~f:(fun x -> sar x imm) op
  | Add (op1, op2) -> concretize2_rw ~offset ~init ~f:add op1 op2
  | Sub (op1, op2) -> concretize2_rw ~offset ~init ~f:sub op1 op2
  | Xor (op1, op2) -> concretize2_rw ~offset ~init ~f:xor op1 op2
  | And (op1, op2) -> concretize2_rw ~offset ~init ~f:and_ op1 op2
  | Or (op1, op2) -> concretize2_rw ~offset ~init ~f:or_ op1 op2
  | Lea (op1, op2) -> concretize2_write ~offset ~init ~f:lea op1 op2
  | Mov (op1, op2) -> concretize2_write ~offset ~init ~f:mov op1 op2
  | Movzx (op1, op2) -> concretize2_write ~offset ~init ~f:movzx op1 op2

let rev_reg_alloc_fn ~offset =
  let f acc x = rev_reg_alloc_instr ~offset acc x in
  List.fold ~f ~init:[]

let reg_alloc_fn fn =
  let offset = Ir.Temp.Virtual.Table.create () in
  let body = List.rev (rev_reg_alloc_fn ~offset fn) in
  let temps = Int64.of_int (Hashtbl.length offset * 8) in
  Enter (temps, 0L) :: body

let reg_alloc_directive = function
  | (Data _ | Globl _ | IntelSyntax _) as dir -> dir
  | Text fns -> Text (List.map ~f:(Asm.Fn.map_body ~f:reg_alloc_fn) fns)

let reg_alloc = List.map ~f:reg_alloc_directive
