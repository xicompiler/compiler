open Core
open Subtype
open Digraph

type nocjump = Lir.expr Subtype.stmt

type stmt =
  [ nocjump
  | `CJump of Lir.expr * label
  ]

type toplevel =
  [ `Func of label * stmt list
  | `Data of label * int64
  ]

type t = toplevel list

(** [label_map nodes] is a map [{l1 : v1, ..., ln : vn}] such that for
    every [i], [li] is bound to [vi] iff [vi] wraps a basic block
    beginning with label [li] *)
let label_map =
  let f acc v =
    match Doubly_linked.first (Vertex.value v) with
    | Some (`Label l) -> Map.set ~key:l ~data:v acc
    | Some _ | None -> acc
  in
  List.fold ~f ~init:String.Map.empty

(** [weight] represents an edge in the control flow graph: either
    labeled or fallthrough *)
type weight =
  | Labeled
  | Fallthrough

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
    | _ -> add_ordinary_edge ~src next
  in
  src |> Vertex.value |> Doubly_linked.last |> Option.iter ~f

(** [add_edges ~labels vs] constructs the cfg by adding edges to every
    vertex in [vs], where [labels] maps labels to the nodes they are
    bound to *)
let rec add_edges ~labels = function
  | src :: vs ->
      add_edge ~src ~labels vs;
      add_edges ~labels vs
  | [] -> ()

(** [break s1 s2] is [true] iff the boundary between [s1] and [s2] must
    be the boundary of a basic block *)
let break s1 s2 =
  match (s1, s2) with
  | (`Return _ | `Jump _ | `CJump _), _ | _, `Label _ -> true
  | _ -> false

(** [cfg_nodes prog] is the list of cfg nodes wrapping maximally-sized
    basic blocks corresponding to [prog] *)
let cfg_nodes prog =
  let f b = Vertex.create (Doubly_linked.of_list b) in
  prog |> List.group ~break |> List.map ~f

(** [create_cfg prog] is list of nodes representing the control flow
    graph of lowered ir program [prog] *)
let create_cfg prog =
  let nodes = cfg_nodes prog in
  let labels = label_map nodes in
  add_edges ~labels nodes;
  nodes

(** [deque_of_cfg cfg] is an [Fdeque.t] containing all nodes of [cfg].
    Vertices with an unmarked predecessor are at the back of the queue,
    and those without are at the front *)
let deque_of_cfg =
  let f t v =
    let side = if Vertex.has_unmarked_pred v then `back else `front in
    Fdeque.enqueue t side v
  in
  List.fold ~f ~init:Fdeque.empty

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
let rev_trace ~start deque = rev_trace_acc [] ~start deque

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
let rev_traces deque = rev_traces_acc [] deque

(** [has_label ~label block] is [true] iff [block] begins with [label] *)
let has_label ~label block =
  match Doubly_linked.first block with
  | Some (`Label l) -> String.equal label l
  | Some _ | None -> false

(** [invert_cjump ~elt ~next e t f] sets the doubly linked list node
    [elt] to CJUMP (!e, f, t) if basic block [next] begins with label
    [t]. Returns: [`Inverted] on inversion, [`Unchanged] if unchanged *)
let invert_cjump ~elt ~next e t f =
  if has_label ~label:t next then begin
    let s = `CJump (log_neg e, f, t) in
    Doubly_linked.Elt.set elt s;
    `Inverted
  end
  else `Unchanged

(** [remove_last v] deletes the last statement from the basic block
    wrapped in [v], if present *)
let remove_last v =
  v |> Vertex.value |> Doubly_linked.remove_last |> ignore

(** [elide_jump ~pred ~succ l] deletes the last statement in the basic
    block of [pred] if [succ] is labeled with [l] *)
let elide_jump ~pred ~succ l =
  let next = Vertex.value succ in
  if has_label ~label:l next then begin
    remove_last pred;
    Vertex.set_edge ~src:pred ~dst:succ ~weight:Fallthrough
  end

(** [push_label ~gensym block] pushes [`Label l] onto the front of
    [block] and returns [l], where [l] is a fresh label. *)
let push_label ~gensym block =
  let label = gensym () in
  let s = `Label label in
  ignore (Doubly_linked.insert_first block s);
  label

(** [find_or_label ~gensym block] is the label that [block] begins with,
    if present, or a fresh label [l] generated by [gensym ()] that is
    pushed onto the front of [block] if absent *)
let find_or_label ~gensym block =
  match Doubly_linked.first block with
  | Some (`Label l) -> l
  | Some _ | None -> push_label ~gensym block

(** [append_jump block l] appends an unconditional jump to label [l]
    onto the end of [block] *)
let append_jump block l =
  let jump = `Jump (`Name l) in
  ignore (Doubly_linked.insert_last block jump)

(** [fix_cjump_fallthrough ~target ~block ~next] appends an
    unconditional jump to [target] if [next] does not begin with label
    [target] *)
let fix_cjump_fallthrough ~target ~curr ~next =
  if not (has_label ~label:target next) then append_jump curr target

(** [add_jump_between ~gensym ~src ~dst] appends to [src] an
    unconditional jump to [dst] and creates an edge [(src, dst)] to
    indicate this control flow, using [gensym] to generate fresh labels
    if needed *)
let add_jump_between ~gensym ~src ~dst =
  Vertex.set_edge ~src ~dst ~weight:Labeled;
  let block = Vertex.value src in
  dst |> Vertex.value |> find_or_label ~gensym |> append_jump block

(** [add_fallthrough_jump  ~gensym u] adds an unconditional jump from
    [u] to an arbitrary successor of [u], if present. Fresh labels are
    generated using [gensym] if needed *)
let add_fallthrough_jump ~gensym u =
  let f succ = add_jump_between ~gensym ~src:u ~dst:succ in
  u |> Vertex.peek_succ |> Option.iter ~f

(** [fix_ordinary_fallthrough  ~gensym ~pred ~succ] fixes the existing
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
let fix_cjump ~pred ~succ ~elt e t f =
  let next = Vertex.value succ in
  let r = invert_cjump ~elt ~next e t f in
  let target = match r with `Inverted -> t | `Unchanged -> f in
  let curr = Vertex.value pred in
  fix_cjump_fallthrough ~target ~curr ~next

(** [fix_jump_stmt ~gensym ~pred ~succ elt] fixes the jumps, if any,
    wrapped in [elt] where [succ] is the basic block following [pred].
    Fresh labels are generated using [gensym], if needed *)
let fix_jump_stmt ~gensym ~pred ~succ elt =
  match Doubly_linked.Elt.value elt with
  | `Jump (`Name l) -> elide_jump ~pred ~succ l
  | `CJump (e, t, f) -> fix_cjump ~pred ~succ ~elt e t f
  | `Return _ | `Jump _ -> ()
  | _ -> fix_ordinary_fallthrough ~gensym ~pred ~succ

(** [fix_jump ~gensym ~pred ~succ] fixes any jumps present in [pred],
    where [succ] is the basic block following [pred]. Fresh labels are
    generated using [gensym], if needed *)
let fix_jump ~gensym ~pred ~succ =
  pred |> Vertex.value |> Doubly_linked.last_elt
  |> Option.iter ~f:(fix_jump_stmt ~gensym ~pred ~succ)

(** [fix_jumps blocks] repairs the jumps of every basic block in
    [blocks]. Fresh labels are generated using [gensym], if needed *)
let rec fix_jumps ~gensym = function
  | [ _ ] | [] -> ()
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
  let block = Vertex.value v in
  match Doubly_linked.first block with
  | Some (`Label l) when not (has_labeled_incoming v) ->
      ignore (Doubly_linked.remove_first block)
  | Some _ | None -> ()

(** [remove_unused_labels vs] deletes unused labels from every basic
    block of [vs] *)
let remove_unused_labels = List.iter ~f:remove_unused_label

(** [remove_false_label s] if [`CJump (e, t)] if [s] is
    [`CJump (e, t, f)] and [s] otherwise *)
let remove_false_label = function
  | `CJump (e, t, _) -> `CJump (e, t)
  | #nocjump as s -> s

(** [concatenated_traces prog] is a sequence of CFG vertices
    corresponding to traces of [prog], with branches between basic
    blocks broken and unused labels present *)
let concatenated_traces prog =
  prog |> create_cfg |> deque_of_cfg |> rev_traces
  |> Util.List.rev_concat

let reorder_stmts stmts ~gensym =
  let traces = concatenated_traces stmts in
  fix_jumps ~gensym traces;
  remove_unused_labels traces;
  traces
  |> List.concat_map ~f:(Vertex.compose ~f:Doubly_linked.to_list)
  |> List.map ~f:remove_false_label

let reorder_toplevel (top : Lir.toplevel) ~gensym : toplevel =
  match top with
  | `Data _ as d -> d
  | `Func (l, b) -> `Func (l, reorder_stmts b ~gensym)

let reorder (prog : Lir.t) ~gensym : t =
  List.map ~f:(reorder_toplevel ~gensym) prog
