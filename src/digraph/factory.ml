open Core
open Option.Monad_infix
open Abstract
open Util.Fn

module Make (Key : Key) = struct
  module Key = Key
  module Table = Hashtbl.Make (Key)

  type ('v, 'e) vertex = {
    key : Key.t;
    mutable value : 'v;
    mutable marked : bool;
    mutable unmarked_pred : int;
    mutable incoming : ('v, 'e) edge list;
    mutable outgoing : ('v, 'e) edge list;
  }

  and ('v, 'e) edge = {
    src : ('v, 'e) vertex;
    dst : ('v, 'e) vertex;
    weight : 'e;
  }

  module Edge = struct
    type ('v, 'e) t = ('v, 'e) edge

    let src { src } = src
    let dst { dst } = dst
    let weight { weight } = weight
  end

  module Vertex = struct
    (** [decr_unmarked_pred v] decrements the number of unmarked
        predecessors of [v]*)
    let decr_unmarked_pred v =
      v.unmarked_pred <- Int.pred v.unmarked_pred

    (** [incr_unmarked_pred v] increments the number of unmarked
        predecessors of [v] *)
    let incr_unmarked_pred v =
      v.unmarked_pred <- Int.succ v.unmarked_pred

    include Graph.Vertex.Make2 (struct
      module Key = Key

      type ('v, 'e) t = ('v, 'e) vertex

      let key { key } = key
      let value { value } = value
      let set v ~value = v.value <- value
      let marked { marked } = marked

      let mark v =
        if not (marked v) then begin
          v.marked <- true;
          let f { dst } = decr_unmarked_pred dst in
          List.iter ~f v.outgoing
        end

      let create ~key ~value =
        {
          key;
          value;
          marked = false;
          unmarked_pred = 0;
          incoming = [];
          outgoing = [];
        }
    end)

    let incoming { incoming } = incoming
    let pred { incoming } = List.rev_map ~f:Edge.src incoming

    (** [fold_pred v ~init ~f] folds function [f] over each predecessor
        of [v] with initial value [init] and returns the result *)
    let fold_pred v ~init ~f =
      List.fold v.incoming ~init ~f:(fun acc -> Edge.src >> f acc)

    (** [add_incoming ~edge v] pushes [edge] onto the incoming edges of
        [v] *)
    let add_incoming ~edge v = v.incoming <- edge :: v.incoming

    let outgoing { outgoing } = outgoing
    let succ { outgoing } = List.rev_map ~f:Edge.dst outgoing

    (** [fold_succ u ~init ~f] folds function [f] over each successor of
        [u] with initial value [init] and returns the result *)
    let fold_succ u ~init ~f =
      List.fold u.outgoing ~init ~f:(fun acc -> Edge.dst >> f acc)

    let has_succ v ~target =
      let f { dst } = equal target dst in
      List.exists ~f v.outgoing

    let peek_succ { outgoing } = List.hd outgoing >>| Edge.dst

    (** [add_outgoing ~edge v] pushes [edge] onto the outgoing edges of
        [v] *)
    let add_outgoing ~edge v = v.outgoing <- edge :: v.outgoing

    let has_unmarked_pred { unmarked_pred } = unmarked_pred > 0

    let add_edge ~src ~dst ~weight =
      let edge = { src; dst; weight } in
      add_incoming ~edge dst;
      add_outgoing ~edge src;
      if unmarked src then incr_unmarked_pred dst

    (** [filter_incoming ~src ~dst] is the list of all edges [(u, dst)]
        with [u] not equal to [src] *)
    let filter_incoming ~src ~dst =
      let f { src = u } = not (equal src u) in
      List.rev_filter ~f dst.incoming

    (** [filter_outgoing ~src ~dst] is the list of all edges [(src, v)]
        with [v] not equal to [dst] *)
    let filter_outgoing ~src ~dst =
      let f { dst = v } = not (equal dst v) in
      List.rev_filter ~f src.outgoing

    (** [remove_outgoing ~src ~dst] deletes all edges [(src, dst)]
        exiting [src] *)
    let remove_outgoing ~src ~dst =
      src.outgoing <- filter_outgoing ~src ~dst

    (** [remove_incoming ~src ~dst] deletes all edges [(src, dst)]
        entering [dst]*)
    let remove_incoming ~src ~dst =
      dst.incoming <- filter_incoming ~src ~dst

    let exists_incoming v ~f = List.exists ~f v.incoming

    let remove_edge ~src ~dst =
      remove_incoming ~src ~dst;
      remove_outgoing ~src ~dst;
      if unmarked src then decr_unmarked_pred dst

    let set_edge ~src ~dst ~weight =
      remove_edge ~src ~dst;
      add_edge ~src ~dst ~weight
  end

  type ('v, 'e) t = ('v, 'e) vertex Table.t

  let create ?size () = Table.create ?size ()

  module Dataflow = struct
    type 'data values = {
      input : 'data;
      output : 'data;
    }
    [@@deriving fields]

    type 'data map = Key.t -> 'data values

    (** [create_data_map ~top g] is a fresh hashtable where every key in
        [g] is bound to [init] *)
    let create_data_map ~top =
      Hashtbl.map ~f:(fun _ -> { input = top; output = top })

    (** [input_nodes ~direction v] is the list of nodes whose dataflow
        values are inputs to the meet operator of [v], i.e. the
        predecessors of [v] if direction is [`Forward] and the
        successors of [v] if direction if [`Backward] *)
    let input_nodes ~direction =
      match direction with
      | `Forward -> Vertex.pred
      | `Backward -> Vertex.succ

    (** [input_data ~direction ~data u] is a list of the dataflow values
        that are the input to the meet function for [u] where the
        dataflow value of a node is resolved via [data]. The input to
        the meet operator are the incoming dataflow values if
        [direction] is [`Forward] and the outgoing dataflow values if
        [direction] is [`Backward] *)
    let input_data ~direction ~data_map =
      let f = Vertex.key >> Hashtbl.find_exn data_map >> output in
      input_nodes ~direction >> List.map ~f

    (** [update_worklist v ~init ~direction] is [init] together with all
        of the nodes whose dataflow values may have been immediately
        impacted by the modification of dataflow values at node [v]; the
        successors of [v] if [direction] is [`Forward], and the
        predecessors of [v] if [direction] is [`Backward] *)
    let update_worklist v ~init ~direction =
      let f acc u = u :: acc in
      match direction with
      | `Forward -> Vertex.fold_succ v ~init ~f
      | `Backward -> Vertex.fold_pred v ~init ~f

    let analyze g Dataflow.Params.{ f; meet; top; direction; equal } =
      (* Initialize dataflow values to top element *)
      let data_map = create_data_map ~top g in
      let rec loop = function
        | [] -> () (* If worklist empty, we're done *)
        | (Vertex.{ key; value } as v) :: t ->
            (* current dataflow value for node v *)
            let { output = l } = Hashtbl.find_exn data_map key in
            (* apply transfer function to meet at node to get updated
               dataflow values *)
            let input = meet (input_data v ~direction ~data_map) in
            let l' = f ~data:input ~vertex:value in
            Hashtbl.set data_map ~key ~data:{ input; output = l' };
            if not (equal l l') then
              (* If dataflow values changed push nodes onto the worklist
                 who dataflow values may have changed *)
              loop (update_worklist v ~init:t ~direction)
            else loop t (* Otherwise, don't need to update worklist *)
      in
      (* Initially, the worklist contains every node *)
      loop (Hashtbl.data g);
      (* Return a function mapping the key of a node to its dataflow
         value *)
      fun k -> Hashtbl.find_exn data_map k
  end

  let graphviz ~string_of_vertex ~string_of_weight nodes =
    let stringify_vertex = Fn.compose String.escaped string_of_vertex in
    let stringify_weight = Fn.compose String.escaped string_of_weight in
    let f node =
      let v1 = stringify_vertex node in
      let out = node.outgoing in
      if List.is_empty out then Printf.sprintf "\"%s\";" v1
      else
        node.outgoing
        |> List.map ~f:(fun e ->
               let v2 = stringify_vertex e.dst in
               let w = stringify_weight e.weight in
               Printf.sprintf "\"%s\" -> \"%s\" [label = \"%s\"];" v1 v2
                 w)
        |> String.concat ~sep:"\n"
    in
    let viz = nodes |> List.map ~f |> String.concat ~sep:"\n" in
    if String.is_empty viz then "empty digraph"
    else
      Printf.sprintf "digraph {\nnode [shape = rectangle];\n\n%s\n}" viz
end
