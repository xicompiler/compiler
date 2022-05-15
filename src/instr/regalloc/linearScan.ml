open Core
open Generic
open Common

module LiveInterval = struct
  type t = {
    reg : Reg.Abstract.t;
    startpoint : int;
    endpoint : int;
  }
  [@@deriving sexp]
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

  (** [equal i1 i2] is whether [i1] and [i2] are associated with the
      same variable *)
  let equal { reg = reg1 } { reg = reg2 } = Reg.Abstract.equal reg1 reg2

  (** [interference_free i1 i2] is whether [i1] and [i2] don't overlap *)
  let interference_free i1 i2 =
    i1.startpoint > i2.endpoint || i1.endpoint < i2.startpoint
end

module Liveness = Dataflow.Liveness.Make (Reg.Abstract.Set)
(** [Liveness] includes the dataflow parameters for liveness analysis *)

(** [analyze cfg] runs a liveness analysis on [cfg] and returns a
    function that maps instruction node key to the set of variables live
    at the instruction node *)
let analyze cfg =
  let params = Liveness.params ~use:Abstract.use ~def:Abstract.def in
  Graph.Directed.IntDigraph.analyze cfg params

(** [intervals_tbl nodes] is a table mapping each variable to their live
    intervals in [nodes] *)
let intervals_tbl nodes =
  let tbl = Reg.Abstract.Table.create () in
  let live = analyze (cfg nodes) in
  let f i node =
    let regs = live i in
    let add reg =
      let data =
        match Hashtbl.find tbl reg with
        | Some interval -> LiveInterval.update interval i
        | None -> LiveInterval.create reg i
      in
      Hashtbl.set tbl ~key:reg ~data
    in
    Set.iter ~f:add regs.output
  in
  List.iteri ~f nodes;
  tbl

(** [intervals tbl] is the data of [intervals] sorted in order of
    ascending startpoint *)
let intervals tbl =
  List.sort ~compare:LiveInterval.compare_start (Hashtbl.data tbl)

(** [update ~concrete ~concrete_inv abstract reg] sets a concretization
    mapping from [abstract] to [reg] *)
let update ~concrete ~concrete_inv abstract reg =
  Hashtbl.set concrete ~key:abstract ~data:reg;
  Hashtbl.set concrete_inv ~key:(reg :> Reg.t) ~data:abstract

(** [update_opt ~concrete ~concrete_inv abstract reg] updates [abstract]
    to [reg'] if [reg] is [Some reg'] *)
let update_opt ~concrete ~concrete_inv abstract =
  Option.iter ~f:(update ~concrete ~concrete_inv abstract)

(** [assign_self ~concrete ~concrete_inv reg] maps the register [reg] to
    itself *)
let assign_self ~concrete ~concrete_inv reg =
  update ~concrete ~concrete_inv (reg :> Reg.Abstract.t) reg

module Pool = struct
  type t = Reg.Bit64.Set.t [@@deriving sexp]

  (** [full] is the full pool of available registers to be allocated *)
  let full =
    Reg.Bit64.Set.of_list [ `rax; `rcx; `rdx; `rsi; `rdi; `r11 ]

  (** [free pool reg] is [pool] with [reg] added if it was originally in
      the pool, or [pool] otherwise *)
  let free pool reg =
    if Set.mem full reg then Set.add pool reg else pool

  (** [choose ~tbl pool interval] is [(reg, pool')] where [reg] is an
      arbitrary register chosen from [pool] where the live range of
      [reg] doesn't interfere with the live range of [interval], and
      [pool'] is the updated pool *)
  let choose ~tbl pool (interval : LiveInterval.t) =
    let f reg =
      match Hashtbl.find tbl (reg :> Reg.Abstract.t) with
      | Some i -> LiveInterval.interference_free i interval
      | None -> true
    in
    let choice = Set.find ~f pool in
    ( choice,
      match choice with Some reg -> Set.remove pool reg | None -> pool
    )
end

(** [free ~pool ~concrete interval] deallocates all the concretized
    registers for [intervals] *)
let free ~pool ~concrete intervals =
  let f pool { LiveInterval.reg } =
    let freed = Hashtbl.find concrete reg in
    Option.value_map ~default:pool ~f:(fun x -> Pool.free pool x) freed
  in
  List.fold ~f ~init:pool intervals

(** [expire_interval ~concrete (active, pool) (key, intervals)] expires
    [intervals] and removes [key] from [active], and returns the updated
    [(active, pool)] *)
let expire_interval ~concrete (active, pool) (key, intervals) =
  (Map.remove active key, free ~pool ~concrete intervals)

(** [expire ~pool ~concrete ~active ~active_concrete interval] finds and
    expires the intervals that are no longer live when the startpoint of
    [interval] is reached *)
let expire
    ~pool
    ~concrete
    ~active
    ~active_concrete
    { LiveInterval.startpoint } =
  (* for each interval j in active less than i's startpoint,
   * remove j from active and free the register allocated to j *)
  let remove =
    Map.range_to_alist ~min:0 ~max:(pred startpoint) active
  in
  let active, pool =
    List.fold ~f:(expire_interval ~concrete) ~init:(active, pool) remove
  in
  let remove =
    Map.range_to_alist ~min:0 ~max:(pred startpoint) active
  in
  let active_concrete, pool =
    List.fold ~f:(expire_interval ~concrete) ~init:(active, pool) remove
  in
  (active, active_concrete, pool)

(** [allocate ~pool ~concrete ~concrete_inv ~active interval] allocates
    a new concretized register for [interval] *)
let allocate
    ~tbl
    ~pool
    ~concrete
    ~concrete_inv
    ~active
    ~active_concrete
    (interval : LiveInterval.t) =
  match interval.reg with
  | #Reg.Bit64.t as reg ->
      assign_self ~concrete ~concrete_inv reg;
      let pool = Set.remove pool reg in
      let active_concrete =
        Map.add_multi active_concrete ~key:interval.endpoint
          ~data:interval
      in
      (active, active_concrete, pool)
  | #Reg.Abstract.t as reg ->
      let reg', pool = Pool.choose ~tbl pool interval in
      let active =
        match reg' with
        | Some reg' ->
            update ~concrete ~concrete_inv reg reg';
            Map.add_multi active ~key:interval.endpoint ~data:interval
        | None -> active
      in
      (active, active_concrete, pool)

(** [spill_interval ~concrete ~concrete_inv interval spill] allocates
    the concretized register for [spill] for the abstract register of
    [interval] *)
let spill_interval
    ~concrete
    ~concrete_inv
    { LiveInterval.reg = interval }
    { LiveInterval.reg = spill } =
  let reg = Hashtbl.find_exn concrete spill in
  Hashtbl.remove concrete spill;
  update ~concrete ~concrete_inv interval reg

(** [spill ~concrete ~concrete_inv ~active interval] spills [interval]
    or the interval with the last endpoint in [active] *)
let spill ~concrete ~concrete_inv ~active (interval : LiveInterval.t) =
  (* get the interval with the last endpoint in active *)
  let key, intervals = Map.max_elt_exn active in
  let last =
    Util.List.max_elt_exn ~compare:LiveInterval.compare_end intervals
  in
  if last.endpoint > interval.endpoint then begin
    (* if the last interval is live for longer than interval i, 
     * spill the last interval *)
    spill_interval ~concrete ~concrete_inv interval last;
    (* remove the last interval from active *)
    let data =
      List.filter intervals ~f:(fun x ->
          not (LiveInterval.equal last x))
    in
    let active =
      if List.is_empty data then Map.remove active key
      else Map.set active ~key ~data
    in
    (* add interval i to active *)
    Map.add_multi active ~key:interval.endpoint ~data:interval
  end
  else active

(** [concrete_tbl nodes] is a table mapping abstract registers to
    concrete registers *)
let concrete_tbl nodes =
  let tbl = intervals_tbl nodes in
  let concrete = Reg.Abstract.Table.create () in
  let concrete_inv = Reg.Table.create () in
  (* foreach liveinterval i, in order of increasing start point *)
  let f (active, active_concrete, pool) (interval : LiveInterval.t) =
    (* expire old intervals at i *)
    let active, active_concrete, pool =
      expire ~pool ~concrete ~active ~active_concrete interval
    in
    match interval.reg with
    | #Reg.Bit64.t ->
        (* a concrete register should always be allocated to itself *)
        allocate ~tbl ~pool ~concrete ~concrete_inv ~active
          ~active_concrete interval
    | #Reg.Abstract.t ->
        if Set.is_empty pool then
          (* if there are no free registers, spill at i *)
          ( spill ~concrete ~concrete_inv ~active interval,
            active_concrete,
            pool )
        else
          (* otherwise, allocate a concrete register to i 
           * and add i to active *)
          allocate ~tbl ~pool ~concrete ~concrete_inv ~active
            ~active_concrete interval
  in
  ignore
    (List.fold ~f
       ~init:(Int.Map.empty, Int.Map.empty, Pool.full)
       (intervals tbl));
  concrete

(** [concretize ~concrete instr] is [instr] concretized as much as
    possible using the [concrete] mapping *)
let concretize ~concrete =
  let f abstract =
    match Hashtbl.find concrete abstract with
    | Some reg -> (reg :> Reg.Abstract.t)
    | None -> abstract
  in
  Abstract.map ~f

(** [rev_allocate_instr ~offset ~concrete ~init instr] is a set of
    reversed instructions concretizing [instr]'s abstract registers *)
let rev_allocate_instr ~offset ~concrete ~init instr =
  let defs = Abstract.def instr in
  let f acc reg =
    match Hashtbl.find concrete reg with
    | Some _ -> acc
    | None -> reg :: acc
  in
  let spills = Set.fold ~f ~init:[] (Abstract.regs instr) in
  let spill op = List.mem ~equal:Reg.Abstract.equal spills op in
  let shuttle = Shuttle.empty in
  let loaded, shuttle =
    rev_allocate_load ~offset ~shuttle ~init spills defs instr
  in
  let concretized =
    Concretize.concretize_instr ~shuttle ~spill
      (concretize ~concrete instr)
  in
  match concretized with
  | Mov ((#Reg.t as e1), (#Reg.t as e2)) when Reg.equal e1 e2 -> init
  | _ ->
      let init = concretized :: loaded in
      rev_allocate_store ~offset ~shuttle ~init spill defs

let allocate_instrs ~offset instrs =
  let nodes = create_cfg instrs in
  let concrete = concrete_tbl nodes in
  let f acc reg = store ~offset ~dst:reg reg :: acc in
  let init = List.fold ~f ~init:[] Shuttle.shuttle in
  let f acc instr =
    rev_allocate_instr ~offset ~concrete ~init:acc instr
  in
  List.rev (List.fold ~f ~init instrs)
