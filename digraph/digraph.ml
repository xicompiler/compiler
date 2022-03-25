open Core
open Option.Monad_infix

type ('v, 'e) vertex = {
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
  type ('v, 'e) t = ('v, 'e) vertex

  let incoming { incoming } = incoming
  let pred { incoming } = List.rev_map ~f:Edge.src incoming

  (** [add_incoming ~edge v] pushes [edge] onto the incoming edges of
      [v] *)
  let add_incoming ~edge v = v.incoming <- edge :: v.incoming

  let outgoing { outgoing } = outgoing
  let succ { outgoing } = List.rev_map ~f:Edge.dst outgoing

  let has_succ v ~target =
    let f { dst } = phys_equal target dst in
    List.exists ~f v.outgoing

  let peek_succ { outgoing } = List.hd outgoing >>| Edge.dst

  (** [add_outgoing ~edge v] pushes [edge] onto the outgoing edges of
      [v] *)
  let add_outgoing ~edge v = v.outgoing <- edge :: v.outgoing

  let value { value } = value
  let set v ~value = v.value <- value
  let map_set v ~f = set v (f v.value)
  let map v ~f = f v.value
  let marked { marked } = marked
  let unmarked v = not (marked v)

  (** [decr_unmarked_pred v] decrements the number of unmarked
      predecessors of [v]*)
  let decr_unmarked_pred v = v.unmarked_pred <- Int.pred v.unmarked_pred

  (** [incr_unmarked_pred v] increments the number of unmarked
      predecessors of [v] *)
  let incr_unmarked_pred v = v.unmarked_pred <- Int.succ v.unmarked_pred

  let mark v =
    v.marked <- true;
    let f { dst } = decr_unmarked_pred dst in
    List.iter ~f v.outgoing

  let has_unmarked_pred { unmarked_pred } = unmarked_pred > 0

  let add_edge ~src ~dst ~weight =
    let edge = { src; dst; weight } in
    add_incoming ~edge dst;
    add_outgoing ~edge src;
    if unmarked src then incr_unmarked_pred dst

  (** [filter_incoming ~src ~dst] is the list of all edges [(u, dst)]
      with [u] not equal to [src] *)
  let filter_incoming ~src ~dst =
    let f { src = u } = not (phys_equal src u) in
    List.rev_filter ~f dst.incoming

  (** [filter_outgoing ~src ~dst] is the list of all edges [(src, v)]
      with [v] not equal to [dst] *)
  let filter_outgoing ~src ~dst =
    let f { dst = v } = not (phys_equal dst v) in
    List.rev_filter ~f src.outgoing

  (** [remove_outgoing ~src ~dst] deletes all edges [(src, dst)] exiting
      [src] *)
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

  let create value =
    {
      value;
      marked = false;
      unmarked_pred = 0;
      incoming = [];
      outgoing = [];
    }
end

let graphviz ~string_of_vertex ~string_of_weight nodes =
  let stringify_vertex = Fn.compose String.escaped string_of_vertex in
  let stringify_weight = Fn.compose String.escaped string_of_weight in
  let f node =
    let v1 = stringify_vertex node in
    node.outgoing
    |> List.map ~f:(fun e ->
           let v2 = stringify_vertex e.dst in
           let w = stringify_weight e.weight in
           Printf.sprintf "\"%s\" -> \"%s\" [label = \"%s\"];" v1 v2 w)
    |> String.concat ~sep:"\n"
  in
  let viz = nodes |> List.map ~f |> String.concat ~sep:"\n" in
  if String.is_empty viz then "empty digraph"
  else Printf.sprintf "digraph g {\nnode [shape = square];\n\n%s\n}" viz
