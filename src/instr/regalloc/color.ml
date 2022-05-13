open Core
open Generic

module InterferenceGraph = struct
  module G = Graph.Undirected.Make (Reg.Abstract)
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
    CFG.foldi_vertices cfg ~init:G.empty ~f
end
