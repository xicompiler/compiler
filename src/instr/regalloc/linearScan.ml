open Core
open Generic
open Common

module LiveInterval = struct
  type t = {
    reg : Reg.Abstract.t;
    startpoint : int;
    endpoint : int;
  }
  [@@deriving equal]
  (** [t] represents a range between [startpoint] and [endpoint] *)

  (** [create reg key] is a new [interval] for [reg] [startpoint]
      initialized to [key] *)
  let create reg key = { reg; startpoint = key; endpoint = key }

  (** [update interval key] is [interval] with [endpoint] updated to
      [key] *)
  let update interval key = { interval with endpoint = key }

  (** [compare_start i1 i2] compares the startpoints of [i1] and [i2] *)
  let compare_start { startpoint = start1 } { startpoint = start2 } =
    Int.compare start1 start2

  (** [compare_end i1 i2] compares the endpoints of [i1] and [i2] *)
  let compare_end { endpoint = end1 } { endpoint = end2 } =
    Int.compare end1 end2
end

module Liveness = Dataflow.Liveness.Make (Reg.Abstract.Set)
(** [Liveness] includes the dataflow parameters for liveness analysis *)

(** [analyze cfg] runs a liveness analysis on [cfg] and returns a
    function that maps instruction node key to the set of variables live
    at the instruction node *)
let analyze cfg =
  let params = Liveness.params ~use:Abstract.use ~def:Abstract.def in
  Graph.Directed.IntDigraph.analyze cfg params

(** [intervals_tbl nodes] is a table mapping each register to their live
    intervals in [nodes] *)
let intervals_tbl nodes =
  let tbl = Reg.Abstract.Table.create () in
  let live = analyze (cfg nodes) in
  let f i node =
    let regs = live i in
    let add reg =
      let data =
        match Reg.Abstract.Table.find tbl reg with
        | Some interval -> LiveInterval.update interval i
        | None -> LiveInterval.create reg i
      in
      Reg.Abstract.Table.set tbl ~key:reg ~data
    in
    Set.iter ~f:add regs.input
  in
  List.iteri ~f nodes;
  tbl

(** [intervals tbl] is the data of [intervals] sorted in order of
    ascending startpoint *)
let intervals tbl =
  List.sort ~compare:LiveInterval.compare_start
    (Reg.Abstract.Table.data tbl)

module Pool = struct
  (** [pool] is the pool of available registers to be allocated *)
  let pool =
    Reg.Abstract.Set.of_list
      [
        `rax; `rbx; `rcx; `rdx; `rsi; `rdi; `r11; `r12; `r13; `r14; `r15;
      ]
end

(** [free ~pool concrete interval] deallocates all the concretized
    registers for [intervals] *)
let free ~pool concrete intervals =
  let f acc { LiveInterval.reg } =
    let freed = Reg.Abstract.Table.find_and_remove concrete reg in
    Option.value_map ~default:acc ~f:(fun x -> Set.add acc x) freed
  in
  List.fold ~f ~init:pool intervals

(** [expire_interval ~concrete (active, pool) (key, intervals)] expires
    [intervals] and removes [key] from [active], and returns the updated
    [(active, pool)] *)
let expire_interval ~concrete (active, pool) (key, intervals) =
  (Map.remove active key, free ~pool concrete intervals)

(** [expire ~pool ~concrete ~active interval] finds and expires the
    intervals that are no longer live when the startpoint of [interval]
    is reached *)
let expire ~pool ~concrete ~active { LiveInterval.startpoint } =
  let remove =
    Map.range_to_alist ~min:0 ~max:(pred startpoint) active
  in
  List.fold ~f:(expire_interval ~concrete) ~init:(active, pool) remove

(** [allocate ~pool ~concrete ~active interval] allocates a new
    concretized register for [interval] *)
let allocate ~pool ~concrete ~active (interval : LiveInterval.t) =
  let data = Set.choose_exn pool in
  let pool = Set.remove pool data in
  Reg.Abstract.Table.add_exn concrete ~key:interval.reg ~data;
  (Map.add_multi active ~key:interval.endpoint ~data:interval, pool)

(** [spill_interval ~concrete interval spill] allocates the concretized
    register for [spill] for the abstract register of [interval] *)
let spill_interval
    ~concrete
    { LiveInterval.reg = key }
    { LiveInterval.reg = data_key } =
  let data = Reg.Abstract.Table.find_exn concrete data_key in
  Reg.Abstract.Table.add_exn concrete ~key ~data

(** [spill ~concrete ~active interval] spills [interval] or the interval
    with the last endpoint in [active] *)
let spill ~concrete ~active (interval : LiveInterval.t) =
  let key, intervals = Map.max_elt_exn active in
  let last =
    Util.List.max_elt_exn ~compare:LiveInterval.compare_end intervals
  in
  if last.endpoint > interval.endpoint then begin
    spill_interval concrete interval last;
    let data =
      List.filter intervals ~f:(fun x ->
          not (LiveInterval.equal last x))
    in
    let active =
      if List.is_empty data then Map.remove active key
      else Map.set active ~key ~data
    in
    Map.add_multi active ~key:interval.endpoint ~data:interval
  end
  else active

(** [concrete_tbl nodes] is a table mapping abstract registers to
    concrete registers *)
let concrete_tbl nodes =
  let tbl = intervals_tbl nodes in
  let concrete = Reg.Abstract.Table.create () in
  let f (active, pool) interval =
    let active, pool = expire ~pool ~concrete ~active interval in
    if Set.is_empty pool then (spill ~concrete ~active interval, pool)
    else allocate ~pool ~concrete ~active interval
  in
  ignore (List.fold ~f ~init:(Int.Map.empty, Pool.pool) (intervals tbl));
  concrete

let concretize ~concrete instrs =
  let f abstract =
    match Reg.Abstract.Table.find concrete abstract with
    | Some reg -> reg
    | None -> abstract
  in
  Abstract.map_list ~f instrs

(** REFACTOR *)
let rec rev_allocate_load ~offset ~shuttle ~init spills instr :
    Concrete.t list * Shuttle.t =
  let f (init, shuttle) src =
    let dst, shuttle =
      if Generic.is_setcc instr then Shuttle.set_bit8 shuttle src
      else Shuttle.set shuttle src
    in
    (load ~offset ~src dst :: init, shuttle)
  in
  List.fold ~init:(init, shuttle) ~f spills

(** REFACTOR *)
let rev_allocate_store ~offset ~shuttle ~init spill = function
  | Some dst when spill dst ->
      let src = Concretize.concretize_reg ~shuttle dst in
      store ~offset ~dst src :: init
  | Some _ | None -> init

let rev_allocate_instr ~offset ~concrete ~init instr =
  let def =
    if Generic.is_call instr then None
    else Reg.Abstract.Set.choose (Abstract.def instr)
  in
  let f acc reg =
    match Reg.Abstract.Table.find concrete reg with
    | Some _ -> acc
    | None -> reg :: acc
  in
  let regs = instr |> Abstract.regs |> Set.to_list in
  let spills = List.fold ~f ~init:[] regs in
  let spill op = List.exists ~f:(Reg.Abstract.equal op) spills in
  let shuttle = Shuttle.empty in
  let loaded, shuttle =
    rev_allocate_load ~offset ~shuttle ~init spills instr
  in
  let concretized =
    Concretize.concretize_instr ~shuttle ~spill instr :: loaded
  in
  rev_allocate_store ~offset ~shuttle ~init:concretized spill def

let allocate_fn ~offset instrs =
  let nodes = create_cfg instrs in
  let concrete = concrete_tbl nodes in
  let concretized = concretize ~concrete instrs in
  let f acc instr =
    rev_allocate_instr ~offset ~concrete ~init:acc instr
  in
  List.rev (List.fold ~f ~init:[] concretized)
