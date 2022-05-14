open Core
open Option.Monad_infix
open Abstract
open Util.Fn

module Make (Key : Key) = struct
  module Key = Key

  type key = Key.t

  module Table = Hashtbl.Make (Key)

  type ('v, 'e) vertex = {
    key : key;
    mutable value : 'v;
    mutable marked : bool;
    mutable unmarked_pred : int;
    mutable prev : ('v, 'e) vertex option;
    mutable next : ('v, 'e) vertex option;
    mutable incoming : ('v, 'e) edge Table.t;
    mutable outgoing : ('v, 'e) edge Table.t;
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
    type ('v, 'e) t = ('v, 'e) vertex

    let incoming { incoming } = Hashtbl.data incoming

    (** [fold_pred v ~init ~f] folds function [f] over each predecessor
        of [v] with initial value [init] and returns the result *)
    let fold_pred v ~init ~f =
      Hashtbl.fold v.incoming ~init ~f:(fun ~key:_ ~data acc ->
          f acc (Edge.src data))

    let pred = fold_pred ~init:[] ~f:(fun acc v -> v :: acc)

    (** [add_incoming ~edge v] pushes [edge] onto the incoming edges of
        [v] *)
    let add_incoming ~edge v =
      Hashtbl.set v.incoming ~key:v.key ~data:edge

    let outgoing { outgoing } = Hashtbl.data outgoing

    (** [fold_succ u ~init ~f] folds function [f] over each successor of
        [u] with initial value [init] and returns the result *)
    let fold_succ v ~init ~f =
      Hashtbl.fold v.outgoing ~init ~f:(fun ~key:_ ~data acc ->
          f acc (Edge.dst data))

    let succ = fold_succ ~init:[] ~f:(fun acc v -> v :: acc)
    let compare v1 v2 = Key.compare v1.key v2.key
    let equal v1 v2 = compare v1 v2 = 0

    let has_succ v ~target =
      let f { dst } = equal target dst in
      Hashtbl.exists ~f v.outgoing

    let peek_succ { outgoing } =
      Hashtbl.choose outgoing >>| (snd >> Edge.dst)

    (** [add_outgoing ~edge v] pushes [edge] onto the outgoing edges of
        [v] *)
    let add_outgoing ~edge v =
      Hashtbl.set v.outgoing ~key:v.key ~data:edge

    let key { key } = key
    let value { value } = value
    let set v ~value = v.value <- value
    let update v ~f = set v (f v.value)
    let fold v ~f = f v.value
    let marked { marked } = marked
    let unmarked v = not (marked v)

    (** [decr_unmarked_pred v] decrements the number of unmarked
        predecessors of [v]*)
    let decr_unmarked_pred v =
      v.unmarked_pred <- Int.pred v.unmarked_pred

    (** [incr_unmarked_pred v] increments the number of unmarked
        predecessors of [v] *)
    let incr_unmarked_pred v =
      v.unmarked_pred <- Int.succ v.unmarked_pred

    let mark v =
      if unmarked v then begin
        v.marked <- true;
        let f { dst } = decr_unmarked_pred dst in
        Hashtbl.iter ~f v.outgoing
      end

    let has_unmarked_pred { unmarked_pred } = unmarked_pred > 0

    let add_edge ~src ~dst ~weight =
      let edge = { src; dst; weight } in
      add_incoming ~edge dst;
      add_outgoing ~edge src;
      if unmarked src then incr_unmarked_pred dst

    let add_unweighted_edge ~src ~dst = add_edge ~src ~dst ~weight:()

    (** [remove_outgoing ~src ~dst] deletes all edges [(src, dst)]
        exiting [src] *)
    let remove_outgoing ~src ~dst = Hashtbl.remove src.outgoing dst.key

    (** [remove_incoming ~src ~dst] deletes all edges [(src, dst)]
        entering [dst]*)
    let remove_incoming ~src ~dst = Hashtbl.remove dst.incoming src.key

    let exists_incoming v ~f = Hashtbl.exists ~f v.incoming

    let remove_edge ~src ~dst =
      remove_incoming ~src ~dst;
      remove_outgoing ~src ~dst;
      if unmarked src then decr_unmarked_pred dst

    let set_edge ~src ~dst ~weight =
      remove_edge ~src ~dst;
      add_edge ~src ~dst ~weight

    let create ~key ~value =
      {
        key;
        value;
        marked = false;
        unmarked_pred = 0;
        prev = None;
        next = None;
        incoming = Table.create ();
        outgoing = Table.create ();
      }

    (** insert node [u] after [prev] *)
    let insert_after u ~prev =
      (* the node to be inserted *)
      let ins = Some u in
      let next = prev.next in
      Option.iter next ~f:(fun next -> next.prev <- ins);
      u.next <- next;
      u.prev <- Some prev;
      prev.next <- ins

    let insert_before u ~next =
      let ins = Some u in
      let prev = next.prev in
      next.prev <- ins;
      u.next <- Some next;
      u.prev <- prev;
      Option.iter prev ~f:(fun prev -> prev.next <- ins)

    (** [link vs] links each successive pair of vertices in [vs] *)
    let rec link = function
      | [ _ ] | [] -> ()
      | u :: (v :: _ as t) ->
          u.next <- Some v;
          v.prev <- Some u;
          link t
  end

  type ('v, 'e) t = {
    mutable head : ('v, 'e) vertex option;
    vertices : ('v, 'e) vertex Table.t;
    max_key : Key.t option;
  }
  [@@deriving fields]

  let create ?size () =
    { head = None; vertices = Table.create ?size (); max_key = None }

  let add_vertex g v =
    let key = Vertex.key v in
    Hashtbl.add_exn g.vertices ~key ~data:v

  let of_vertices vs =
    Vertex.link vs;
    {
      head = List.hd vs;
      vertices = Table.create_with_key_exn ~get_key:Vertex.key vs;
      max_key = List.max_elt vs ~compare:Vertex.compare >>| Vertex.key;
    }

  let foldi_vertices g ~init ~f =
    let f ~key ~data acc = f key acc data in
    Hashtbl.fold g.vertices ~init ~f

  let iteri_vertices g ~f =
    foldi_vertices g ~init:() ~f:(fun i _ v -> f i v)

  let iter_vertices g ~f = iteri_vertices g ~f:(fun _ -> f)

  let insert_after g v ~prev =
    add_vertex g v;
    Vertex.insert_after v ~prev

  let insert_before g v ~next =
    add_vertex g v;
    Vertex.insert_before v ~next;
    (* The head must be present because we're inserting before an
       existing node *)
    let head = Option.value_exn g.head in
    (* If we're inserting before the head, this node is the new head *)
    if Vertex.equal head next then g.head <- Some v

  let splice_before g v ~succ =
    (* Insert v before succ in linked list *)
    insert_before g v ~next:succ;
    (* Edges entering succ *)
    let incoming = succ.incoming in
    Hashtbl.map_inplace incoming ~f:(fun (Edge.{ src } as e) ->
        (* Remove succ from adjacent nodes of predecessors *)
        Hashtbl.remove src.outgoing succ.key;
        (* Copy same edge but with destination as spliced node *)
        let e = Edge.{ e with dst = v } in
        (* Create an edge (src, v) *)
        Hashtbl.add_exn src.outgoing ~key:v.key ~data:e;
        e);
    (* Set incoming edges to modified incoming edges of succ *)
    v.incoming <- incoming;
    (* reset the incoming edges of succ *)
    succ.incoming <- Table.create ();
    (* add edge (v, succ) *)
    Vertex.add_unweighted_edge ~src:v ~dst:succ

  let splice_after g v ~pred =
    insert_after g v ~prev:pred;
    (* Edges exiting pred *)
    let outgoing = pred.outgoing in
    Hashtbl.map_inplace outgoing ~f:(fun (Edge.{ dst } as e) ->
        (* Remove pred from adjacent nodes of successors *)
        Hashtbl.remove dst.incoming pred.key;
        (* Copy same edge but with source as spliced node *)
        let e = Edge.{ e with src = v } in
        (* Create edge (v, dst) *)
        Hashtbl.add_exn dst.incoming ~key:v.key ~data:e;
        e);
    (* Set outgoing edges to modified outgoing edges of succ *)
    v.outgoing <- outgoing;
    (* reset the outgoing edges of pred *)
    pred.outgoing <- Table.create ();
    (* add edge (pred, v) *)
    Vertex.add_unweighted_edge ~src:pred ~dst:v

  open Dataflow.Values

  (** [create_data_map ~top g] is a fresh hashtable where every key in
      [g] is bound to [init] *)
  let create_data_map ~top =
    Hashtbl.map ~f:(fun _ -> { input = top; output = top })

  (** [input_nodes ~direction v] is the list of nodes whose dataflow
      values are inputs to the meet operator of [v], i.e. the
      predecessors of [v] if direction is [`Forward] and the successors
      of [v] if direction if [`Backward] *)
  let input_nodes ~direction =
    match direction with
    | `Forward -> Vertex.pred
    | `Backward -> Vertex.succ

  (** [input_data ~direction ~data u] is a list of the dataflow values
      that are the input to the meet function for [u] where the dataflow
      value of a node is resolved via [data]. The input to the meet
      operator are the incoming dataflow values if [direction] is
      [`Forward] and the outgoing dataflow values if [direction] is
      [`Backward] *)
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

  let analyze
      { vertices = g }
      Dataflow.Params.{ f; meet; top; direction; equal } =
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
    Hashtbl.find_exn data_map

  let graphviz ~string_of_vertex ~string_of_weight nodes =
    let stringify_vertex = Fn.compose String.escaped string_of_vertex in
    let stringify_weight = Fn.compose String.escaped string_of_weight in
    let f node =
      let v1 = stringify_vertex node in
      let out = Vertex.outgoing node in
      if List.is_empty out then Printf.sprintf "\"%s\";" v1
      else
        out
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
