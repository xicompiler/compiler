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

(** [rev_allocate_load ~offset ~shuttle ~init ~spills instr] loads
    [spills instr] into the appropriate shuttling registers *)
let rec rev_allocate_load ~offset ~shuttle ~init ~spills instr :
    Concrete.t list * Shuttle.t =
  let f (init, shuttle) src =
    let dst, shuttle =
      if Generic.is_setcc instr then Shuttle.set_bit8 shuttle src
      else Shuttle.set shuttle src
    in
    (load ~offset ~src dst :: init, shuttle)
  in
  List.fold ~init:(init, shuttle) ~f (spills instr)

(** [rev_allocate_store ~offset ~shuttle ~init ~spill dst] stores the
    value of the shuttling register of [dst] into the memory address of
    [dst], if [dst] is a spill *)
let rev_allocate_store ~offset ~shuttle ~init ~spill dst =
  if spill dst then
    let src = Concretize.concretize_reg ~shuttle dst in
    store ~offset ~dst src :: init
  else init