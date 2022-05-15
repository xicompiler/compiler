open Core
open Generic
open Common

module Spill = struct
  type t =
    [ Ir.Temp.Virtual.t
    | Shuttle.regs
    ]
  [@@deriving equal]
  (** [t] is a spill for trivial register allocation*)

  (** [spill ?init e] is [e] added to [init] if [e] is a new spill, and
      [init] otherwise *)
  let rec spill ?(init = []) : Operand.Abstract.t -> t list = function
    | #t as spill -> Util.List.add_unique ~equal spill init
    | `Mem mem ->
        let spill ~init x = spill ~init (x :> Operand.Abstract.t) in
        let init =
          Option.map ~f:Mem.Index.index (Mem.index mem)
          |> Option.value_map ~default:init ~f:(spill ~init)
        in
        spill ~init (Mem.base mem)
    | #Operand.Abstract.t -> init

  (** [spill2 ?init e1 e2] is [e1] and [e2] added to [init] if they are
      new spills *)
  let spill2 e = spill ~init:(spill e)

  (** [spills instr] is the list of unique spills used by [instr] *)
  let spills : Abstract.t -> t list = function
    | Label _ | Enter _ | Jcc _ | Leave | Ret _ -> []
    | Jmp e
    | Call { name = e }
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
end

(** [rev_allocate_instr ~offset ~init instr] is a set of reversed
    instructions concretizing [instr]'s abstract registers *)
let rev_allocate_instr ~offset ~init instr =
  let defs = Abstract.def instr in
  let spills = (Spill.spills instr :> Reg.Abstract.t list) in
  let spill op = List.mem ~equal:Reg.Abstract.equal spills op in
  let shuttle = Shuttle.empty in
  let loaded, shuttle =
    rev_allocate_load ~offset ~shuttle ~init spills defs instr
  in
  let concretized =
    Concretize.concretize_instr ~shuttle ~spill instr :: loaded
  in
  rev_allocate_store ~offset ~shuttle ~init:concretized spill defs

let allocate_instrs ~(offset : int Reg.Abstract.Table.t) instrs =
  let f acc reg = store ~offset ~dst:reg reg :: acc in
  let init = List.fold ~f ~init:[] Shuttle.shuttle in
  let f acc instr = rev_allocate_instr ~offset ~init:acc instr in
  List.rev (List.fold ~f ~init instrs)
