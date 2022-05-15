open Core
open Generic

(** [address_of ~offset t] is the memory operand of spill [t] in stack
    space, depending on [alloc] *)
let address_of ~offset t =
  let default () = succ (Hashtbl.length offset) in
  let off = Hashtbl.find_or_add offset t ~default in
  let off = Int64.of_int (~-8 * off) in
  Mem.create ~offset:off `rbp

let load ~offset ~src = function
  | #Reg.Bit64.t as dst ->
      let mem = `Mem (address_of ~offset src) in
      Mov ((dst :> Operand.t), mem)
  | _ -> failwith "cannot load into abstract dst"

let store ~offset ~dst = function
  | #Reg.Bit64.t as src ->
      let mem = `Mem (address_of ~offset dst) in
      Mov (mem, (src :> Operand.t))
  | _ -> failwith "cannot store from abstract src"

let rev_allocate_load ~offset ~shuttle ~init spills defs instr =
  let init =
    if Generic.is_call instr then
      let f acc reg = load ~offset ~src:reg reg :: acc in
      List.fold ~f ~init Shuttle.shuttle
    else init
  in
  let f (init, shuttle) src =
    let dst, shuttle =
      if Generic.is_setcc instr then Shuttle.set_bit8 shuttle src
      else Shuttle.set shuttle src
    in
    if Set.mem defs src && Generic.skip_load instr then (init, shuttle)
    else (load ~offset ~src dst :: init, shuttle)
  in
  List.fold ~init:(init, shuttle) ~f spills

let rev_allocate_store ~offset ~shuttle ~init spill defs =
  let f acc = function
    | dst when spill dst ->
        let src = Concretize.concretize_reg ~shuttle dst in
        store ~offset ~dst src :: acc
    | _ -> acc
  in
  Set.fold ~f ~init defs
