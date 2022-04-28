open Core
open Generic
open Asm.Directive
open Util.Fn

(** [Shuttle] represents the shuttling registers and convenient
    functions for each abstract instruction translation *)
module Shuttle = struct
  type regs =
    [ `r8
    | `r9
    | `r10
    ]
  [@@deriving equal]

  let reg_bit64 = [ `r8; `r9; `r10 ]
  let reg_bit8 = `r8b
  let shuttle = reg_bit64

  type t = {
    regs : Reg.t list;
    map : Reg.t Reg.Abstract.Map.t;
  }

  let regs { regs } = regs
  let map { map } = map

  let set { regs; map } abstract =
    match Map.find map abstract with
    | Some (#Reg.t as reg) -> (reg, { regs; map })
    | None ->
        let reg, tl = Util.List.pop_exn regs in
        (reg, { regs = tl; map = Map.set map ~key:abstract ~data:reg })

  let set_bit8 { regs; map } abstract =
    match Map.find map abstract with
    | Some (#Reg.t as reg) -> (reg, { regs; map })
    | None ->
        let reg = Reg.to_64_bit reg_bit8 in
        (reg, { regs; map = Map.set map ~key:abstract ~data:reg })

  let find_default { map } abstract : Reg.t =
    match Map.find map abstract with
    | Some (#Reg.t as reg) -> reg
    | None -> begin
        match abstract with
        | #Reg.t as reg -> reg
        | _ -> failwith "no value returned in shuttle map"
      end

  let find_bit8_exn { map } abstract =
    match Map.find map abstract with
    | Some (#Reg.t as reg) -> Reg.to_8_bit reg
    | _ -> failwith "not a bit8 reg"

  let empty = { regs = reg_bit64; map = Reg.Abstract.Map.empty }
end

module Spill = struct
  type t =
    [ Ir.Temp.Virtual.t
    | Shuttle.regs
    ]
  [@@deriving equal]
end

let rec spill ?(init = []) : Operand.Abstract.t -> Spill.t list =
  function
  | #Spill.t as t -> Util.List.add_unique ~equal:Spill.equal t init
  | `Mem mem ->
      let spill ~init x = spill ~init (x :> Operand.Abstract.t) in
      let init =
        Option.map ~f:Mem.Index.index (Mem.index mem)
        |> Option.value_map ~default:init ~f:(spill ~init)
      in
      spill ~init (Mem.base mem)
  | #Operand.Abstract.t -> init

let spill2 e = spill ~init:(spill e)

(** [spills instr] is the list of unique spills used by [instr] *)
let spills : Abstract.t -> Spill.t list = function
  | Label _ | Enter _ | Jcc _ | Leave | Ret -> []
  | Jmp e
  | Call e
  | Setcc (_, e)
  | Push e
  | Pop e
  | Inc e
  | Dec e
  | IDiv e
  | Shl (e, _)
  | Shr (e, _)
  | Sar (e, _)
  | IMul (`M e) ->
      spill e
  | Cmp (e1, e2)
  | Test (e1, e2)
  | Add (e1, e2)
  | Sub (e1, e2)
  | Xor (e1, e2)
  | And (e1, e2)
  | Or (e1, e2)
  | Mov (e1, e2)
  | Movzx (e1, e2)
  | Lea (e1, e2)
  | IMul (`RM (e1, e2) | `RMI (e1, e2, _)) ->
      spill2 e2 e1

let concretize_reg ~shuttle = Shuttle.find_default shuttle

let concretize_index ~shuttle = function
  | None -> None
  | Some scaled ->
      let index = concretize_reg ~shuttle (Mem.Index.index scaled) in
      Some (Mem.Index.with_index ~index scaled)

let concretize_mem ~shuttle mem =
  let base = concretize_reg ~shuttle (Mem.base mem) in
  let index = concretize_index ~shuttle (Mem.index mem) in
  Mem.with_registers ?index ~base mem

let concretize_operand ~shuttle : Operand.Abstract.t -> Operand.t =
  function
  | #Spill.t as reg -> (concretize_reg ~shuttle reg :> Operand.t)
  | (#Reg.t | `Name _ | `Imm _) as concrete -> concrete
  | `Mem mem -> `Mem (concretize_mem ~shuttle mem :> Reg.t Mem.generic)

let concretize_map ~shuttle ~f op = f (concretize_operand ~shuttle op)

let concretize2_map ~shuttle ~f op1 op2 =
  let e1 = concretize_operand ~shuttle op1 in
  let e2 = concretize_operand ~shuttle op2 in
  f e1 e2

let concretize_mul ~shuttle = function
  | `M op ->
      let f x = imul (`M x) in
      concretize_map ~shuttle ~f op
  | `RM (op1, op2) ->
      let f x y = imul (`RM (x, y)) in
      concretize2_map ~shuttle ~f op1 op2
  | `RMI (op1, op2, imm) ->
      let f x y = imul (`RMI (x, y, imm)) in
      concretize2_map ~shuttle ~f op1 op2

let concretize_bit8 ~shuttle ~f = function
  | #Spill.t as op ->
      let reg8 = Shuttle.find_bit8_exn shuttle op in
      f reg8
  | #Reg.Bit8.t as reg8 -> f reg8
  | #Operand.Abstract.t ->
      failwith "unexpected operand for 8bit instruction"

let concretize_movzx ~shuttle op1 op2 =
  let op = concretize_operand ~shuttle op1 in
  concretize_bit8 ~shuttle ~f:(movzx op) op2

let rev_concretize_instr ~shuttle : Abstract.t -> Concrete.t = function
  | (Label _ | Enter _ | Jcc _ | Leave | Ret) as instr -> instr
  | Jmp op -> concretize_map ~shuttle ~f:jmp op
  | Setcc (cc, op) -> concretize_bit8 ~shuttle ~f:(setcc cc) op
  | Cmp (op1, op2) -> concretize2_map ~shuttle ~f:cmp op1 op2
  | Test (op1, op2) -> concretize2_map ~shuttle ~f:test op1 op2
  | Push op -> concretize_map ~shuttle ~f:push op
  | Pop op -> concretize_map ~shuttle ~f:pop op
  | IMul mul -> concretize_mul ~shuttle mul
  | Inc op -> concretize_map ~shuttle ~f:inc op
  | Dec op -> concretize_map ~shuttle ~f:dec op
  | Call op -> concretize_map ~shuttle ~f:call op
  | IDiv op -> concretize_map ~shuttle ~f:idiv op
  | Shl (op, imm) -> concretize_map ~shuttle ~f:(fun x -> shl x imm) op
  | Shr (op, imm) -> concretize_map ~shuttle ~f:(fun x -> shr x imm) op
  | Sar (op, imm) -> concretize_map ~shuttle ~f:(fun x -> sar x imm) op
  | Add (op1, op2) -> concretize2_map ~shuttle ~f:add op1 op2
  | Sub (op1, op2) -> concretize2_map ~shuttle ~f:sub op1 op2
  | Xor (op1, op2) -> concretize2_map ~shuttle ~f:xor op1 op2
  | And (op1, op2) -> concretize2_map ~shuttle ~f:and_ op1 op2
  | Or (op1, op2) -> concretize2_map ~shuttle ~f:or_ op1 op2
  | Lea (op1, op2) -> concretize2_map ~shuttle ~f:lea op1 op2
  | Mov (op1, op2) -> concretize2_map ~shuttle ~f:mov op1 op2
  | Movzx (op1, op2) -> concretize_movzx ~shuttle op1 op2

(** [address_of ~offset t] is the memory operand of spill [t] in stack
    space, depending on [alloc] *)
let address_of ~offset t =
  let default () = succ (Hashtbl.length offset) in
  let off = Hashtbl.find_or_add offset t ~default in
  let off = Int64.of_int (~-8 * off) in
  Mem.create ~offset:off `rbp

let load ~offset ~src = function
  | #Reg.t as dst ->
      let mem = `Mem (address_of ~offset src) in
      Mov ((dst :> Operand.t), mem)
  | _ -> failwith "cannot load into abstract dst"

(** [rev_reg_alloc_load ~offset ~shuttle ~init ?def instr] loads the
    values of all temporaries used by [instr] into the appropriate
    shuttling registers *)
let rec rev_reg_alloc_load ~offset ~shuttle ~init ?def instr :
    Concrete.t list * Shuttle.t =
  let abstract = spills instr in
  let init =
    if Generic.is_call instr then
      let f acc reg = load ~offset ~src:reg reg :: acc in
      List.fold ~f ~init Shuttle.shuttle
    else init
  in
  let f (init, shuttle) = function
    | #Spill.t as t
      when match def with
           | Some (#Spill.t as t') ->
               Spill.equal t t' && Generic.skip_load instr
           | _ -> false ->
        let _, shuttle =
          if Generic.is_setcc instr then Shuttle.set_bit8 shuttle t
          else Shuttle.set shuttle t
        in
        (init, shuttle)
    | #Spill.t as src ->
        let dst, shuttle =
          if Generic.is_setcc instr then Shuttle.set_bit8 shuttle src
          else Shuttle.set shuttle src
        in
        (load ~offset ~src dst :: init, shuttle)
  in
  List.fold ~init:(init, shuttle) ~f abstract

let store ~offset ~dst = function
  | #Reg.t as src ->
      let mem = `Mem (address_of ~offset dst) in
      Mov (mem, (src :> Operand.t))
  | _ -> failwith "cannot store from abstract src"

(** [rev_reg_alloc_store ~offset ~shuttle ~init instr operand] stores
    the value of the shuttling register of [operand] into the memory
    address of [operand], if [operand] is a spill *)
let rev_reg_alloc_store ~offset ~shuttle ~init = function
  | Some (#Reg.Bit8.t as dst) ->
      let src = Shuttle.find_bit8_exn shuttle dst in
      store ~offset ~dst src :: init
  | Some (#Spill.t as dst) ->
      let src = concretize_reg ~shuttle dst in
      store ~offset ~dst src :: init
  | Some #Operand.Abstract.t | None -> init

let rev_reg_alloc_instr ~offset ~init instr =
  let def = Generic.def instr in
  let shuttle = Shuttle.empty in
  let loaded, shuttle =
    rev_reg_alloc_load ~offset ~shuttle ~init ?def instr
  in
  let concretized = rev_concretize_instr ~shuttle instr :: loaded in
  let t = rev_reg_alloc_store ~offset ~shuttle ~init:concretized def in
  t

let rev_reg_alloc_fn ~offset =
  let f acc reg = store ~offset ~dst:reg reg :: acc in
  let init = List.fold ~f ~init:[] Shuttle.shuttle in
  let f acc instr = rev_reg_alloc_instr ~offset ~init:acc instr in
  List.fold ~f ~init

let reg_alloc_fn fn =
  let offset = Reg.Abstract.Table.create () in
  let body = List.rev (rev_reg_alloc_fn ~offset fn) in
  let temps = Int64.of_int (Hashtbl.length offset * 8) in
  Enter (temps, 0L) :: body

let reg_alloc_directive = function
  | (Data _ | Globl _ | IntelSyntax _) as dir -> dir
  | Text fns -> Text (List.map ~f:(Asm.Fn.map_body ~f:reg_alloc_fn) fns)

let reg_alloc = List.map ~f:reg_alloc_directive
