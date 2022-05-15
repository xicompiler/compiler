open Core
open Generic

let concretize_reg ~shuttle op = Shuttle.find_default shuttle op

(** [concretize_index ~shuttle idx] concretizes abstract index option
    [idx] *)
let concretize_index ~shuttle = function
  | None -> None
  | Some scaled ->
      let index = concretize_reg ~shuttle (Mem.Index.index scaled) in
      Some (Mem.Index.with_index ~index scaled)

(** [concretize_mem ~shuttle mem] concretizes abstract memory operand
    [mem] *)
let concretize_mem ~shuttle mem =
  let base = concretize_reg ~shuttle (Mem.base mem) in
  let index = concretize_index ~shuttle (Mem.index mem) in
  Mem.with_registers ?index ~base mem

(** [concretize_operand ~shuttle ~spill op] concretizes abstract operand
    [op] *)
let concretize_operand ~shuttle ~spill :
    [< Operand.Abstract.t ] -> Operand.t = function
  | #Reg.Abstract.t as reg when spill reg ->
      (concretize_reg ~shuttle reg :> Operand.t)
  | (#Reg.Bit64.t | `Name _ | `Imm _) as concrete -> concrete
  | `Mem mem -> `Mem (concretize_mem ~shuttle mem :> Reg.t Mem.generic)
  | #Reg.Abstract.t -> failwith "abstract reg should not be concretized"

(** [concretize_map ~shuttle ~spill ~f op] is [f e], where [e] is the
    concretized abstract operand [op] *)
let concretize_map ~shuttle ~spill ~f op =
  f (concretize_operand ~shuttle ~spill op)

(** [concretize2_map ~shuttle ~spill ~f op1 op2] is [f e1 e2], where
    [e1] and [e2] are the concretized abstract operands [op1] and [op2] *)
let concretize2_map ~shuttle ~spill ~f op1 op2 =
  let e1 = concretize_operand ~shuttle ~spill op1 in
  let e2 = concretize_operand ~shuttle ~spill op2 in
  f e1 e2

(** [concretize_mul ~shuttle ~spill mul] concretizes abstract mul
    instruction [op] *)
let concretize_mul ~shuttle ~spill = function
  | `M op ->
      let f x = imul (`M x) in
      concretize_map ~shuttle ~spill ~f op
  | `RM (op1, op2) ->
      let f x y = imul (`RM (x, y)) in
      concretize2_map ~shuttle ~spill ~f op1 op2
  | `RMI (op1, op2, imm) ->
      let f x y = imul (`RMI (x, y, imm)) in
      concretize2_map ~shuttle ~spill ~f op1 op2

(** [concretize_bit8 ~shuttle ~f bit8] is [f e] where [e] is the
    concretized abstract bit8 register [bit8] *)
let concretize_bit8 ~shuttle ~f = function
  | #Reg.Abstract.t as op ->
      let reg8 = Shuttle.find_bit8_exn shuttle op in
      f reg8
  | #Operand.Abstract.t ->
      failwith "unexpected operand for 8bit instruction"

(** [concretize_movzx ~shuttle ~spill op1 op2] concretizes an abstract
    movzx instruction with operands [op1] and [op2] *)
let concretize_movzx ~shuttle ~spill op1 op2 =
  let op = concretize_operand ~shuttle ~spill op1 in
  concretize_bit8 ~shuttle ~f:(movzx op) op2

let concretize_instr ~shuttle ~spill : Abstract.t -> Concrete.t =
  function
  | (Label _ | Enter _ | Jcc _ | Leave | Ret _) as instr -> instr
  | Jmp op -> concretize_map ~shuttle ~spill ~f:jmp op
  | Setcc (cc, op) -> concretize_bit8 ~shuttle ~f:(setcc cc) op
  | Cmp (op1, op2) -> concretize2_map ~shuttle ~spill ~f:cmp op1 op2
  | Test (op1, op2) -> concretize2_map ~shuttle ~spill ~f:test op1 op2
  | Push op -> concretize_map ~shuttle ~spill ~f:push op
  | Pop op -> concretize_map ~shuttle ~spill ~f:pop op
  | IMul mul -> concretize_mul ~shuttle ~spill mul
  | Inc op -> concretize_map ~shuttle ~spill ~f:inc op
  | Dec op -> concretize_map ~shuttle ~spill ~f:dec op
  | Call { name = op; n; m } ->
      concretize_map ~shuttle ~spill
        ~f:(fun x -> call { name = x; n; m })
        op
  | IDiv op -> concretize_map ~shuttle ~spill ~f:idiv op
  | Shl (op, imm) ->
      concretize_map ~shuttle ~spill ~f:(fun x -> shl x imm) op
  | Shr (op, imm) ->
      concretize_map ~shuttle ~spill ~f:(fun x -> shr x imm) op
  | Sar (op, imm) ->
      concretize_map ~shuttle ~spill ~f:(fun x -> sar x imm) op
  | Add (op1, op2) -> concretize2_map ~shuttle ~spill ~f:add op1 op2
  | Sub (op1, op2) -> concretize2_map ~shuttle ~spill ~f:sub op1 op2
  | Xor (op1, op2) -> concretize2_map ~shuttle ~spill ~f:xor op1 op2
  | And (op1, op2) -> concretize2_map ~shuttle ~spill ~f:and_ op1 op2
  | Or (op1, op2) -> concretize2_map ~shuttle ~spill ~f:or_ op1 op2
  | Lea (op1, op2) -> concretize2_map ~shuttle ~spill ~f:lea op1 op2
  | Mov (op1, op2) -> concretize2_map ~shuttle ~spill ~f:mov op1 op2
  | Movzx (op1, op2) -> concretize_movzx ~shuttle ~spill op1 op2
