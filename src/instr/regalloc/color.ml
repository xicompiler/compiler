open Core
open Generic
open Util.Fn
module G = Graph.Undirected.Make (Reg.Abstract)

module InterferenceGraph = struct
  open Dataflow.Values

  (** Add interference edges between the variables defined at a node and
      those live-out *)
  let add_def_edges g v live =
    Set.fold (Abstract.def v) ~init:g ~f:(fun g u ->
        let g = G.add_vertex g u in
        G.add_edges g u live)

  (** [remove s op] is [s - { r }] if [op] is abstract register [r], or
      just [s] if [op] is not an abstract operand *)
  let remove s = function
    | #Reg.Abstract.t as t -> Set.remove s t
    | #Operand.Abstract.t -> s

  (** Add the interfence edges between caller-save registers and temps
      live across call *)
  let add_call_edges init across =
    Set.fold across ~init ~f:(fun g u ->
        G.add_edges g u Reg.Bit64.caller_save)

  (** [create cfg ~live] is the interference graph of control flow graph
      [cfg], where liveness information for variables is looked up using
      [live] *)
  let create cfg ~live =
    let f key g v =
      (* Input to live transfer function is live-out, output is
         live-in *)
      let { input = exit; output = entry } = live key in
      let exit_seq = Set.to_sequence exit in
      (* All variables in the same live-out set interfere *)
      let g = G.add_clique g exit_seq in
      match CFG.Vertex.value v with
      | Call _ as v ->
          (* Call might define some return registers *)
          let g = add_def_edges g v exit_seq in
          (* Live-out vars also interfere with caller-saved registers.
             The set of vars live across the call is those that are live
             in and live out. *)
          add_call_edges g (Set.inter entry exit)
      | Mov (_, src) as v ->
          (* For move instruction x <- y, we create interference edges
             (x, z) where z is live out, except for edge (x, y) *)
          let intf = Set.to_sequence (remove exit src) in
          add_def_edges g v intf
      | v -> add_def_edges g v exit_seq
    in
    List.foldi cfg ~init:G.empty ~f
end

module Virtual = Ir.Temp.Virtual

(** [find_spill ~gensym] is a mapping from temporaries to spill
    temporaries *)
let find_spill ~gensym =
  let tbl = Ir.Temp.Virtual.Table.create ~size:3 () in
  Hashtbl.find_or_add tbl ~default:gensym

(** [replace instr ~src ~dst] replaces every instance of register [src]
    with register [dst] *)
let replace instr ~src ~dst =
  Abstract.map instr ~f:(fun r ->
      if Reg.Abstract.equal src r then dst else r)

(** [rewrite instrs ~spills ~addr ~gensym] is [instrs] rewritten such
    that every occurrence of some [t] is replaced by a shuttle to/from
    the stack *)
let rewrite instrs ~spills ~addr ~gensym : Abstract.t list =
  let f acc instr =
    (* function mapping temporaries to their spill temporaries *)
    let find_spill = find_spill ~gensym in
    (* Get a pair (instr, acc) where [instr] is the rewritten
       instruction and [acc] is the list of instructions needed to
       shuttle from/to stack *)
    let rewrite ?(init = []) ~instr ~f =
      Set.fold ~init:(instr, init) ~f:(fun ((instr, lst) as acc) ->
        function
        | #Virtual.t as t when Set.mem spills t ->
            (* If variable is spilled to stack, lookup stack location *)
            let addr = SpillAddress.find addr t in
            (* the temporary used to shuttle from/to stack *)
            let spill = `Temp (find_spill t) in
            (* replace all occurences of spilled temporary with newly
               generated one*)
            let instr = replace instr ~src:t ~dst:spill in
            (instr, f spill (`Mem addr) :: lst)
        | _ -> acc)
    in
    let use = Abstract.use instr in
    let def = Abstract.def instr in
    (* for uses, shuttle off stack into registers *)
    let instr, from_stk = rewrite use ~instr ~init:acc ~f:Generic.mov in
    (* for defs, shuttle from register back to stack *)
    let instr, to_stk = rewrite def ~instr ~f:(Fn.flip Generic.mov) in
    (* sandwich rewritten instruction in between moves from the stack
       and writes back to the stack. Append is fast because few
       instructions shuttling back to stack *)
    to_stk @ (instr :: from_stk)
  in
  (* Instructions produced in reverse order *)
  instrs |> List.fold ~init:[] ~f |> List.rev

module Live = Dataflow.Liveness.Make (Reg.Abstract.Set)

let params = Live.params ~use:Abstract.use ~def:Abstract.def

let filter_dead =
  List.filter_map ~f:(function
    | Mov ((#Reg.t as r1), (#Reg.t as r2)) when Reg.equal r1 r2 -> None
    | Setcc (cc, (#Reg.t as r)) -> Some (Setcc (cc, Reg.to_8_bit r))
    | Movzx (dst, (#Reg.t as src)) ->
        Some (Movzx (dst, Reg.to_8_bit src))
    | instr -> Some instr)

let map_abstract ~f = function
  | #Ir.Temp.Virtual.t as t -> (f t :> Reg.t)
  | #Reg.Bit64.t as concrete -> concrete

let replace_abstract instrs ~color ~frame =
  let replace = map_abstract ~f:(color >> Reg.Bit64.of_int_exn) in
  let size = Int64.of_int (8 * frame) in
  let concrete = Abstract.map_concrete_list instrs ~f:replace in
  Enter (size, 0L) :: filter_dead concrete

let assign instrs ~gensym =
  let addr = SpillAddress.create () in
  let rec loop (instrs : Abstract.t list) prev =
    let vs = Generic.create_cfg instrs in
    let cfg = CFG.of_vertices vs in
    let live = CFG.analyze cfg params in
    let intf = InterferenceGraph.create ~live vs in
    match G.kempe ~precolor:Reg.Abstract.to_int intf ~max:9 with
    | Error spills ->
        let instrs = rewrite instrs ~spills ~addr ~gensym in
        loop instrs (Set.union prev spills)
    | Ok color ->
        let frame = SpillAddress.count addr in
        replace_abstract instrs ~color ~frame
  in
  loop instrs G.Set.empty
