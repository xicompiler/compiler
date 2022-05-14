open Core

type regs =
  [ `r8
  | `r9
  | `r10
  ]
[@@deriving equal]

(** [reg_bit64] is the 64-bit shuttling registers for a single
    instruction *)
let reg_bit64 = [ `r8; `r9; `r10 ]

(** [reg_bit8] is the 8-bit shuttling registers for a single instruction *)
let reg_bit8 = `r8b

let shuttle = reg_bit64

type t = {
  regs : Reg.t list;
  mapping : Reg.t Reg.Abstract.Map.t;
}

let set { regs; mapping } abstract =
  match Map.find mapping abstract with
  | Some (#Reg.t as reg) -> (reg, { regs; mapping })
  | None ->
      let reg, tl = Util.List.pop_exn regs in
      ( reg,
        { regs = tl; mapping = Map.set mapping ~key:abstract ~data:reg }
      )

let set_bit8 { regs; mapping } abstract =
  match Map.find mapping abstract with
  | Some (#Reg.t as reg) -> (reg, { regs; mapping })
  | None ->
      let reg = Reg.to_64_bit reg_bit8 in
      (reg, { regs; mapping = Map.set mapping ~key:abstract ~data:reg })

let find_default { mapping } abstract : Reg.t =
  match Map.find mapping abstract with
  | Some (#Reg.t as reg) -> reg
  | None -> begin
      match abstract with
      | #Reg.Bit64.t as reg -> reg
      | _ -> failwith "no value returned in shuttle map"
    end

let find_bit8_exn { mapping } abstract =
  match Map.find mapping abstract with
  | Some reg -> Reg.to_8_bit reg
  | _ -> failwith "not a bit8 reg"

let empty = { regs = reg_bit64; mapping = Reg.Abstract.Map.empty }