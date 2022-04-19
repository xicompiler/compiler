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
    | `r8b
    ]
  [@@deriving equal]

  let reg_bit64 = [ `r8; `r9; `r10 ]
  let reg_bit8 = `r8b
  let shuttle = reg_bit8 :: reg_bit64

  type t = {
    regs : Reg.t list;
    map : Reg.Abstract.t Reg.Abstract.Map.t;
  }

  let regs { regs } = regs
  let map { map } = map

  let set { regs; map } abstract =
    let reg, tl = Util.List.pop_exn regs in
    ( reg,
      {
        regs = tl;
        map = Map.set map ~key:abstract ~data:(reg :> Reg.Abstract.t);
      } )

  let set_bit8 { regs; map } abstract =
    (reg_bit8, { regs; map = Map.set map ~key:abstract ~data:reg_bit8 })

  let find_default { map } abstract : Reg.t =
    match Map.find map abstract with
    | Some (#Reg.t as reg) -> reg
    | Some #Ir.Temp.Virtual.t ->
        failwith "not a valid value in shuttle map"
    | None -> begin
        match abstract with
        | #Reg.t as reg -> reg
        | _ -> failwith "no value returned in shuttle map"
      end

  let find_bit8_exn { map } abstract =
    match Map.find map abstract with
    | Some (#Reg.Bit8.t as reg) -> reg
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
  | #Spill.t as t -> t :: init
  | `Mem mem ->
      let spill ~init x = spill ~init (x :> Operand.Abstract.t) in
      let init =
        Option.map ~f:Mem.Index.index (Mem.index mem)
        |> Option.value_map ~default:init ~f:(spill ~init)
      in
      spill ~init (Mem.base mem)
  | #Operand.Abstract.t -> init

let spill2 e = spill ~init:(spill e)

(** [spills instr] is the list of spills used by [instr] *)
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

let concretize_setcc ~shuttle ~init cc = function
  | #Spill.t as op ->
      let reg8 = Shuttle.find_bit8_exn shuttle op in
      Movzx (Reg.Bit8.to_64_bit reg8, reg8) :: Setcc (cc, reg8) :: init
  | #Reg.Bit8.t as reg8 ->
      Movzx (Reg.Bit8.to_64_bit reg8, reg8) :: Setcc (cc, reg8) :: init
  | #Operand.Abstract.t -> failwith "unexpected operand for setcc"

let rev_concretize_instr ~shuttle ~init : Abstract.t -> Concrete.t list
    = function
  | (Label _ | Enter _ | Jcc _ | Leave | Ret) as instr -> instr :: init
  | Jmp op -> concretize_map ~shuttle ~f:jmp op :: init
  | Setcc (cc, op) -> concretize_setcc ~shuttle ~init cc op
  | Cmp (op1, op2) -> concretize2_map ~shuttle ~f:cmp op1 op2 :: init
  | Test (op1, op2) -> concretize2_map ~shuttle ~f:test op1 op2 :: init
  | Push op -> concretize_map ~shuttle ~f:push op :: init
  | Pop op -> concretize_map ~shuttle ~f:pop op :: init
  | IMul mul -> concretize_mul ~shuttle mul :: init
  | Inc op -> concretize_map ~shuttle ~f:inc op :: init
  | Dec op -> concretize_map ~shuttle ~f:dec op :: init
  | Call op -> concretize_map ~shuttle ~f:call op :: init
  | IDiv op -> concretize_map ~shuttle ~f:idiv op :: init
  | Shl (op, imm) ->
      concretize_map ~shuttle ~f:(fun x -> shl x imm) op :: init
  | Shr (op, imm) ->
      concretize_map ~shuttle ~f:(fun x -> shr x imm) op :: init
  | Sar (op, imm) ->
      concretize_map ~shuttle ~f:(fun x -> sar x imm) op :: init
  | Add (op1, op2) -> concretize2_map ~shuttle ~f:add op1 op2 :: init
  | Sub (op1, op2) -> concretize2_map ~shuttle ~f:sub op1 op2 :: init
  | Xor (op1, op2) -> concretize2_map ~shuttle ~f:xor op1 op2 :: init
  | And (op1, op2) -> concretize2_map ~shuttle ~f:and_ op1 op2 :: init
  | Or (op1, op2) -> concretize2_map ~shuttle ~f:or_ op1 op2 :: init
  | Lea (op1, op2) -> concretize2_map ~shuttle ~f:lea op1 op2 :: init
  | Mov (op1, op2) -> concretize2_map ~shuttle ~f:mov op1 op2 :: init
  | Movzx (op1, op2) ->
      concretize2_map ~shuttle ~f:movzx op1 op2 :: init

(** [address_of ~offset ?size t] is the memory operand of temp [t] in
    stack space, depending on [alloc] *)
let address_of ~offset ?(size = Mem.Size.Qword) t =
  let default () = succ (Hashtbl.length offset) in
  let off = Hashtbl.find_or_add offset t ~default in
  let off = Int64.of_int (~-8 * off) in
  Mem.create ~offset:off ~size `rbp

let load ~offset ~src dst =
  let size =
    match dst with #Reg.Bit8.t -> Some Mem.Size.Byte | _ -> None
  in
  let mem = `Mem (address_of ~offset ?size src) in
  Mov ((dst :> Operand.t), mem)

(** [rev_reg_alloc_load ~offset ~shuttle ~init ?def instr] loads the
    values of all temporaries used by [instr] into the appropriate
    shuttling registers *)
let rev_reg_alloc_load ~offset ~shuttle ~init ?def instr =
  let abstract = spills instr in
  let f (init, shuttle) = function
    | #Spill.t as t
      when false
           &&
           match def with
           | Some (#Spill.t as t') ->
               Spill.equal t t' && Generic.skip_load instr (* TODO *)
           | _ -> false ->
        (init, shuttle)
    | #Spill.t as src ->
        let dst, shuttle =
          if Generic.is_setcc instr then Shuttle.set_bit8 shuttle src
          else Shuttle.set shuttle src
        in
        (load ~offset ~src dst :: init, shuttle)
  in
  List.fold ~init:(init, shuttle) ~f abstract

let store ~offset ~src dst =
  let size =
    match src with #Reg.Bit8.t -> Some Mem.Size.Byte | _ -> None
  in
  let mem = `Mem (address_of ~offset ?size dst) in
  Mov (mem, (src :> Operand.t))

(** [rev_reg_alloc_store ~offset ~shuttle ~init instr operand] stores
    the value of the shuttling register of [operand] into the memory
    address of [operand], if [operand] is a spill *)
let rev_reg_alloc_store ~offset ~shuttle ~init instr = function
  | Some (#Spill.t as dst) when Generic.is_setcc instr ->
      let reg = concretize_reg ~shuttle dst in
      let src = Reg.to_64_bit reg in
      store ~offset ~src dst :: init
  | Some (#Spill.t as dst) ->
      let src = concretize_reg ~shuttle dst in
      store ~offset ~src dst :: init
  | Some #Operand.Abstract.t | None -> init

let rev_reg_alloc_instr ~offset ~init instr =
  let def = Generic.def instr in
  let shuttle = Shuttle.empty in
  let loaded, shuttle =
    rev_reg_alloc_load ~offset ~shuttle ~init ?def instr
  in
  let concretized = rev_concretize_instr ~shuttle ~init:loaded instr in
  rev_reg_alloc_store ~offset ~shuttle ~init:concretized instr def

let rev_reg_alloc_fn ~offset =
  let f acc reg =
    store ~offset ~src:reg (reg :> Reg.Abstract.t) :: acc
  in
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
