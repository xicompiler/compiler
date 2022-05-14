open Core
open Generic

(** [pool] is the pool of available registers to be allocated *)
let pool =
  [ `rbx; `rcx; `rdx; `rsi; `rdi; `r11; `r12; `r13; `r14; `r15 ]

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

(** [expire_interval ~concrete (active, pool) (key, intervals)] expires
    [intervals] and removes [key] from [active], and returns the updated
    [(active, pool)] *)
let expire_interval ~concrete (active, pool) (key, intervals) =
  let f acc { LiveInterval.reg } =
    let freed = Reg.Abstract.Table.find_exn concrete reg in
    Reg.Abstract.Table.remove concrete reg;
    freed :: acc
  in
  (Map.remove active key, List.fold ~f ~init:pool intervals)

(** [expire ~pool ~concrete ~active interval] expires the intervals that
    are old when the startpoint of [interval] is reached *)
let expire ~pool ~concrete ~active { LiveInterval.startpoint } =
  let remove = Map.range_to_alist ~min:0 ~max:startpoint active in
  List.fold ~f:(expire_interval ~concrete) ~init:(active, pool) remove

(** [allocate ~pool concrete interval] allocates a new concretized
    register for [interval] *)
let allocate ~pool concrete { LiveInterval.reg = key } =
  let data, pool = Util.List.pop_exn pool in
  Reg.Abstract.Table.add_exn concrete ~key ~data;
  pool

(** [allocate_spill concrete interval spill] allocates the concretized
    register for [spill] to the abstract register of [interval] *)
let allocate_spill
    concrete
    { LiveInterval.reg = key }
    { LiveInterval.reg = data_key } =
  let data = Reg.Abstract.Table.find_exn concrete data_key in
  Reg.Abstract.Table.add_exn concrete ~key ~data

(** [spill ~concrete ~active interval] spills [interval] or the interval
    with the last endpoint in [active] *)
let spill ~concrete ~active (interval : LiveInterval.t) =
  let key, intervals = Map.max_elt_exn active in
  let spill =
    Util.List.max_elt_exn ~compare:LiveInterval.compare_end intervals
  in
  if spill.endpoint > interval.endpoint then begin
    allocate_spill concrete interval spill;
    let active =
      Map.set active ~key
        ~data:(List.filter intervals ~f:(LiveInterval.equal spill))
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
    if List.is_empty pool then (spill ~concrete ~active interval, pool)
    else
      let pool = allocate ~pool concrete interval in
      let active =
        Map.add_multi active ~key:interval.endpoint ~data:interval
      in
      (active, pool)
  in
  ignore (List.fold ~f ~init:(Int.Map.empty, pool) (intervals tbl));
  concrete

let allocate_fn ~offset instrs =
  let nodes = create_cfg instrs in
  let concrete = concrete_tbl nodes in
  let f abstract =
    match Reg.Abstract.Table.find concrete abstract with
    | Some reg -> reg
    | None -> failwith ""
  in
  Abstract.map_concrete ~f instrs
