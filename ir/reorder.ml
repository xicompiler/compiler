open Core
open Subtype
open Util.Option
open Digraph.Make (Int)

type nocjump = Lir.expr Subtype.stmt

type stmt =
  [ nocjump
  | `CJump of Lir.expr * label
  ]

type toplevel =
  [ `Func of label * stmt list
  | `Data of label * int64 list
  ]

type t = toplevel list

(** [label_map nodes] is a map [{l1 : v1, ..., ln : vn}] such that for
    every [i], [li] is bound to [vi] iff [vi] wraps a basic block
    beginning with label [li] *)
let label_map =
  let f acc v =
    match Vertex.map v ~f:BasicBlock.first with
    | Some (`Label l) -> Map.set ~key:l ~data:v acc
    | Some _ | None -> acc
  in
  List.fold ~f ~init:String.Map.empty

(** [weight] represents an edge in the control flow graph: either
    labeled or fallthrough *)
type weight =
  | Labeled
  | Fallthrough

(** [string_of_weight weight] is the string representaiton of [weight] *)
let string_of_weight = function
  | Labeled -> "labeled"
  | Fallthrough -> "fallthrough"

(** [string_of_vertex vertex] is the string representation of the value
    of [vertex] *)
let string_of_vertex v = v |> Vertex.value |> BasicBlock.to_string

(** [is_labeled Labeled] is [true], [is_labeled Fallthrough] is [false] *)
let is_labeled = function Labeled -> true | Fallthrough -> false

(** [add_jump_edge ~src ~labels l] adds an edge [(src, dst)] if [dst] is
    the vertex labeled with [l] *)
let add_jump_edge ~src ~labels l =
  let dst = Map.find_exn labels l in
  Vertex.add_edge ~src ~dst ~weight:Labeled

(** [add_cjump_edge ~src ~labels l1 l2] adds edges [(src, t)] and
    [(src, f)] where [t] is the vertex bound to [l1] in [map] and [f] is
    the vertex bound to [l2] in [map] *)
let add_cjump_edge ~src ~labels l1 l2 =
  let t = Map.find_exn labels l1 in
  Vertex.add_edge ~src ~dst:t ~weight:Labeled;
  let f = Map.find_exn labels l2 in
  Vertex.add_edge ~src ~dst:f ~weight:Labeled

(** [add_ordinary_edge ~src next] adds an edge [(src, dst)] where [dst]
    is the first vertex of [next], if present. Does nothing if [next] is
    empty *)
let add_ordinary_edge ~src = function
  | dst :: _ -> Vertex.add_edge ~src ~dst ~weight:Fallthrough
  | [] -> ()

(** [add_edge ~src ~labels next s] correctly adds every edge exiting CFG
    vertex [src] where [map] maps labels to the node containing those
    labels and [next] is the list of following basic blocks *)
let add_edge ~src ~labels next =
  let f = function
    | `Jump (`Name l) -> add_jump_edge ~src ~labels l
    | `CJump (_, l1, l2) -> add_cjump_edge ~src ~labels l1 l2
    | `Return _ | `Jump _ -> ()
    | #Lir.stmt -> add_ordinary_edge ~src next
  in
  src |> Vertex.map ~f:BasicBlock.last |> Option.iter ~f

(** [add_edges ~labels vs] constructs the cfg by adding edges to every
    vertex in [vs], where [labels] maps labels to the nodes they are
    bound to *)
let rec add_edges ~labels = function
  | src :: vs ->
      add_edge ~src ~labels vs;
      add_edges ~labels vs
  | [] -> ()

(** [cfg_nodes prog] is the list of cfg nodes wrapping maximally-sized
    basic blocks corresponding to [prog] *)
let cfg_nodes =
  let f key value = Vertex.create ~key ~value in
  Fn.compose (List.mapi ~f) BasicBlock.of_lir

(** [create_cfg prog] is list of nodes representing the control flow
    graph of lowered ir program [prog] *)
let create_cfg prog =
  let nodes = cfg_nodes prog in
  let labels = label_map nodes in
  add_edges ~labels nodes;
  nodes

(** [traverse ~visited v] is a mapping of keys to vertices not in
    [visited] that results in a traversal from [v] *)
let rec traverse ~visited v =
  let key = Vertex.key v in
  let f acc = traverse ~visited:acc in
  match Map.add visited ~key ~data:v with
  | `Ok init -> v |> Vertex.succ |> List.fold ~f ~init
  | `Duplicate -> visited

(** [enqueue_vertex t v] is [t] with [v] pushed to the front if it has
    no unmarked predecessors, and [t] with [v] pushed to the back
    otherwise *)
let enqueue_vertex t v =
  let side = if Vertex.has_unmarked_pred v then `back else `front in
  Fdeque.enqueue t side v

(** [create_deque start] is an [Fdeque.t] containing all nodes reachable
    from [start]. Vertices with an unmarked predecessor are at the back
    of the queue, and those without are at the front *)
let create_deque start =
  let f ~key:_ ~data acc = enqueue_vertex acc data in
  start
  |> traverse ~visited:Int.Map.empty
  |> Map.fold ~f ~init:Fdeque.empty

(** [enqueue_all deque vs] is [deque] with all unmarked vertices of [v]
    with no unmarked predecessors enqueued to the front *)
let enqueue_all deque =
  let f t v =
    if Vertex.unmarked v && not (Vertex.has_unmarked_pred v) then
      Fdeque.enqueue_front t v
    else t
  in
  List.fold ~init:deque ~f

(** [rev_trace_acc acc ~start deque] is a trace starting from [start]
    where [deque] describes the remaining unmarked vertices, ending with
    [acc] in reverse order *)
let rec rev_trace_acc acc ~start deque =
  Vertex.mark start;
  let succ = Vertex.succ start in
  let acc = start :: acc in
  match List.find ~f:Vertex.unmarked succ with
  | Some v -> rev_trace_acc acc ~start:v (enqueue_all deque succ)
  | None -> (deque, acc)

(** [rev_trace ~start deque] is [rev_trace_acc \[\] ~start deque]*)
let rev_trace ~start = rev_trace_acc [] ~start

(** [rev_traces_acc acc deque] is [acc] preceded by a sequence of
    maximal traces consisting of unmarked vertices from [deque] *)
let rec rev_traces_acc acc deque =
  match Fdeque.dequeue_front deque with
  | None -> acc
  | Some (h, t) ->
      if Vertex.marked h then rev_traces_acc acc t
      else
        let deque, trace = rev_trace ~start:h t in
        rev_traces_acc (trace :: acc) deque

(** [rev_traces deque] is a sequences of traces consisting of unmarked
    vertices from [deque] *)
let rev_traces = rev_traces_acc []

(** [invert_cjump ~pred ~succ e t f] sets the doubly linked list node
    [pred] to CJUMP (!e, f, t) if basic block [next] begins with label
    [t]. Returns: [`Inverted] on inversion, [`Unchanged] if unchanged *)
let invert_cjump ~pred ~succ e t f =
  if Vertex.map ~f:(BasicBlock.has_label ~label:t) succ then begin
    let stmt = `CJump (log_neg e, f, t) in
    Vertex.map_set pred ~f:(BasicBlock.set_last ~stmt);
    `Inverted
  end
  else `Unchanged

(** [elide_jump ~pred ~succ l] deletes the last statement in the basic
    block of [pred] if [succ] is labeled with [l] *)
let elide_jump ~pred ~succ label =
  if Vertex.map succ ~f:(BasicBlock.has_label ~label) then begin
    Vertex.map_set pred ~f:BasicBlock.remove_last;
    Vertex.set_edge ~src:pred ~dst:succ ~weight:Fallthrough
  end

(** [append_jump vertex l] appends an unconditional jump to label [l]
    onto the end of [vertex] *)
let append_jump vertex ~target =
  let jump = `Jump (`Name target) in
  Vertex.map_set vertex ~f:(BasicBlock.insert_last ~stmt:jump)

(** [fix_cjump_fallthrough ~target ~block ~succ] appends an
    unconditional jump to [target] if [next] does not begin with label
    [target] *)
let fix_cjump_fallthrough ~target ~pred ~succ =
  if not (Vertex.map ~f:(BasicBlock.has_label ~label:target) succ) then
    append_jump pred ~target

(** [label v ~gensym] is the label of the basic block wrapped in [v] if
    present. If absent, a fresh label [l] is generated using [gensym],
    pushed to the front of the basic block of [v], and returned. *)
let label v ~gensym =
  let default () =
    let label = gensym () in
    Vertex.map_set v ~f:(BasicBlock.insert_label ~label);
    label
  in
  v |> Vertex.map ~f:BasicBlock.label |> Lazy.value ~default

(** [add_jump_between ~gensym ~src ~dst] appends to [src] an
    unconditional jump to [dst] and creates an edge [(src, dst)] to
    indicate this control flow, using [gensym] to generate fresh labels
    if needed *)
let add_jump_between ~gensym ~src ~dst =
  Vertex.set_edge ~src ~dst ~weight:Labeled;
  let target = label dst ~gensym in
  append_jump src ~target

(** [add_fallthrough_jump ~gensym u] adds an unconditional jump from [u]
    to an arbitrary successor of [u], if present. Fresh labels are
    generated using [gensym] if needed *)
let add_fallthrough_jump ~gensym u =
  let f succ = add_jump_between ~gensym ~src:u ~dst:succ in
  u |> Vertex.peek_succ |> Option.iter ~f

(** [fix_ordinary_fallthrough ~gensym ~pred ~succ] fixes the existing
    fallthrough from [pred] to [succ] by adding an unconditional jump
    from [pred] to one of its successors if [pred] does not yield
    control flow to [succ]. Fresh labels are generated using [gensym] if
    needed *)
let fix_ordinary_fallthrough ~gensym ~pred ~succ =
  if not (Vertex.has_succ pred ~target:succ) then
    add_fallthrough_jump ~gensym pred

(** [fix_cjump ~pred ~succ ~elt e t f] fixes the CJUMP wrapped in [elt]
    where elt wraps [`CJump (e, t, f)] by inverting the condition if
    [succ] is labeled with [t] and fixes the fallthrough if it does not
    yield control to the next basic block *)
let fix_cjump ~pred ~succ e t f =
  let r = invert_cjump ~pred ~succ e t f in
  let target = match r with `Inverted -> t | `Unchanged -> f in
  fix_cjump_fallthrough ~target ~pred ~succ

(** [fix_jump_stmt ~gensym ~pred ~succ stmt] fixes the jumps, if any,
    wrapped in [elt] where [succ] is the basic block following [pred].
    Fresh labels are generated using [gensym], if needed *)
let fix_jump_stmt ~gensym ~pred ~succ = function
  | `Jump (`Name l) -> elide_jump ~pred ~succ l
  | `CJump (e, t, f) -> fix_cjump ~pred ~succ e t f
  | `Return _ | `Jump _ -> ()
  | #Lir.stmt -> fix_ordinary_fallthrough ~gensym ~pred ~succ

(** [iter_last ~f ~gensym pred] is [()] if the basic block of [pred]
    contains no statements, or is [f s'], where [s'] is the last
    statement in the block, ohterwise. *)
let iter_last ~f ~gensym pred =
  pred |> Vertex.map ~f:BasicBlock.last |> Option.iter ~f

(** [fix_jump ~gensym ~pred ~succ] fixes any jumps present in [pred],
    where [succ] is the basic block following [pred]. Fresh labels are
    generated using [gensym], if needed *)
let fix_jump ~gensym ~pred ~succ =
  iter_last ~f:(fix_jump_stmt ~gensym ~pred ~succ) ~gensym pred

(** [fix_jump_singleton_stmt ~gensym ~pred stmt] fixes the jump in
    [stmt], which is the last statement of the basic block of [pred],
    generating fresh symbols using [gensym] if needed *)
let fix_jump_singleton_stmt ~gensym ~pred = function
  | `CJump (_, _, f) -> append_jump pred ~target:f
  | `Return _ | `Jump _ -> ()
  | #Lir.stmt -> add_fallthrough_jump ~gensym pred

(** [fix_jump_singleton ~gensym pred] fixes the jumps following a
    singleton basic block [pred], generating fesh symbols using [gensym]
    if ndeeded *)
let fix_jump_singleton ~gensym ~pred =
  iter_last ~f:(fix_jump_singleton_stmt ~gensym ~pred) ~gensym pred

(** [fix_jumps blocks] repairs the jumps of every basic block in
    [blocks]. Fresh labels are generated using [gensym], if needed *)
let rec fix_jumps ~gensym = function
  | [] -> ()
  | [ pred ] -> fix_jump_singleton ~gensym ~pred
  | pred :: (succ :: _ as t) ->
      fix_jump ~gensym ~pred ~succ;
      fix_jumps ~gensym t

(** [has_labeled_incoming v] is [true] iff [v] has an incoming edge with
    weight [Labeled] *)
let has_labeled_incoming =
  Vertex.exists_incoming ~f:(Fn.compose is_labeled Edge.weight)

(** [remove_unused_label v] removes the label beginning the block
    wrapped in [v] if it is present and [v] has no incoming labeled
    edges *)
let remove_unused_label v =
  match Vertex.map v ~f:BasicBlock.first with
  | Some (`Label l) when not (has_labeled_incoming v) ->
      Vertex.map_set ~f:BasicBlock.remove_first v
  | Some _ | None -> ()

(** [remove_unused_labels vs] deletes unused labels from every basic
    block of [vs] *)
let remove_unused_labels = List.iter ~f:remove_unused_label

(** [remove_false_label s] if [`CJump (e, t)] if [s] is
    [`CJump (e, t, f)] and [s] otherwise *)
let remove_false_label = function
  | `CJump (e, t, _) -> `CJump (e, t)
  | #nocjump as s -> s

(** [concatenated_traces cfg] is a sequence of CFG vertices
    corresponding to traces of [cfg], with branches between basic blocks
    broken and unused labels present *)
let concatenated_traces cfg =
  cfg |> List.hd_exn |> create_deque |> rev_traces
  |> Util.List.rev_concat

(** [print_cfg ~label cfg] prints [label], along with the graphviz code
    for displaying [cfg] *)
let print_cfg ~label cfg =
  print_endline label;
  cfg |> graphviz ~string_of_vertex ~string_of_weight |> print_endline

let reorder_stmts ~gensym stmts =
  let start = `Label (gensym ()) in
  let cfg = create_cfg (start :: stmts) in
  let traces = concatenated_traces cfg in
  fix_jumps ~gensym traces;
  remove_unused_labels traces;
  traces
  |> List.concat_map ~f:(Vertex.map ~f:BasicBlock.to_list)
  |> List.map ~f:remove_false_label

let reorder_toplevel ~gensym (top : Lir.toplevel) : toplevel =
  match top with
  | `Data _ as d -> d
  | `Func (l, b) -> `Func (l, reorder_stmts ~gensym b)

let reorder ~gensym = List.map ~f:(reorder_toplevel ~gensym)
