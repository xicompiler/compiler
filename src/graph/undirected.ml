open Core
open Result.Monad_infix
open Result.Let_syntax

module type Key = Map.Key

module type S = sig
  module Key : Key

  type t

  val empty : t
  val add_vertex : t -> Key.t -> t
  val add_vertices : t -> Key.t Sequence.t -> t
  val add_edge : t -> Key.t -> Key.t -> t
  val add_edges : t -> Key.t -> Key.t Sequence.t -> t
  val add_clique : t -> Key.t Sequence.t -> t

  module Set : Set.S with type Elt.t = Key.t

  val kempe :
    ?spills:Set.t ->
    ?precolor:(Key.t -> int option) ->
    t ->
    max:int ->
    ((Key.t -> int) * int, Set.t) result

  val of_edges : (Key.t * Key.t) list -> t
end

module Make (Key : Key) = struct
  module Key = Key
  module C = Comparable.Make (Key)
  module Map = C.Map
  module Set = C.Set

  module Vertex = struct
    type t = {
      key : Key.t;
      adjacent : Set.t;
    }
    [@@deriving fields]

    let map_adjacent v k ~f = { v with adjacent = f v.adjacent k }
    let add_adjacent = map_adjacent ~f:Set.add
    let remove_adjacent = map_adjacent ~f:Set.remove
    let degree { adjacent } = Set.length adjacent
    let create key = { key; adjacent = Set.empty }
  end

  type t = Vertex.t Map.t

  let empty = Map.empty

  let add_vertex g key : t =
    let v = Vertex.create key in
    (* If there is already a vertex bound to this key, we don't want to
       change the map *)
    match Map.add g ~key ~data:v with `Ok g' -> g' | `Duplicate -> g

  let add_vertices init = Sequence.fold ~init ~f:add_vertex
  let update_exn = Util.Map.update_exn ~message:"no vertex with key"

  let update_vertex ~f (g : t) u v : t =
    update_exn g u ~f:(fun vertex -> f vertex v)

  let add_adjacent g u v =
    Map.update g u ~f:(function
      | Some u -> Vertex.add_adjacent u v
      | None -> Vertex.{ key = u; adjacent = Set.singleton v })

  let remove_adjacent g u v =
    update_exn g u ~f:(fun vertex -> Vertex.remove_adjacent vertex v)

  let add_edge g u v =
    if Key.compare u v = 0 then g
    else
      let g' = add_adjacent g u v in
      add_adjacent g' v u

  let add_edges init u = Sequence.fold ~init ~f:(fun g -> add_edge g u)

  let add_clique init keys =
    let with_nodes = add_vertices init keys in
    Sequence.fold keys ~init:with_nodes ~f:(fun init u ->
        add_edges init u keys)

  let remove_vertex g Vertex.{ key = u; adjacent } =
    let f acc v = remove_adjacent acc v u in
    (* Remove the key of this vertex from the neighbors of all
       neighoring vertices *)
    let g' = Set.fold adjacent ~init:g ~f in
    (* and then remove this vertex from the graph *)
    Map.remove g' u

  let enqueue_all w = Set.fold ~f:Fdeque.enqueue_back ~init:w

  (** Create the initial map binding the keys of precolored nodes to
      their colors. Also, create the worklist of nodes that have to be
      colored; exactly those that are not precolored. Low-degree nodes
      should be selected first, so they precede all high-degree nodes in
      the queue. *)
  let create_map_worklist g ~spills ~max ~precolor =
    let init = (Map.empty, Fdeque.empty) in
    let f ~key ~data ((m, w) as acc) =
      if Set.mem spills key then acc
      else
        match precolor key with
        | Some c ->
            (* If this node is precolored, add it to the precolored map,
               but leave the worklist unchanged; don't color already
               colored nodes *)
            (Map.add_exn m ~key ~data:c, w)
        | None ->
            (* Otherwise, this vertex isn't precolored so we have to
               color it, so we add to worklist. *)
            let side =
              if Vertex.degree data < max then `front else `back
            in
            (* Low-degree nodes at the front, high-degree at the back *)
            (m, Fdeque.enqueue w side key)
    in
    let m, w = Map.fold g ~init ~f in
    (m, enqueue_all w spills)

  let update_worklist g vs ~precolor ~max ~init =
    Set.fold vs ~init ~f:(fun acc key ->
        let v = Map.find_exn g key in
        (* If this vertex has degree exactly max, then deleting exactly
           one neighbor will cause it to become a low-degree node *)
        let color = Map.find precolor key in
        if Option.is_none color && Vertex.degree v = max then
          Fdeque.enqueue_front acc key
        else acc)

  let choose_color m adj ~max =
    (* Start out with all possible colors *)
    let init = Util.BitSet.range ~max in
    let f init key =
      let c = Map.find m key in
      (* If this neighbor is colored, remove its color from the set of
         those available *)
      Option.fold c ~init ~f:(fun acc c -> Util.BitSet.remove c acc)
    in
    (* Choose the minimum color available afterwards, if any *)
    adj |> Set.fold ~init ~f |> Util.BitSet.min

  let update_coloring m ~key ~spills ~color =
    match color with
    | Some c ->
        (* If a color is avaiable, update the coloring mapping to
           reflect this *)
        (Map.add_exn m ~key ~data:c, spills)
    | None ->
        (* Otherwise, we can't color this node so we add it to the
           spills *)
        (m, Set.add spills key)

  (** Get the mapping binding keys to the colors of their respective
      nodes, and also a list of spilled vertices unable to be colored. *)
  let rec color g worklist ~max ~precolor =
    match Fdeque.dequeue_front worklist with
    | None ->
        (* If the worklist is empty, there are no nodes to color, so the
           mapping is just the mapping for precolored nodes, and there
           are no spills *)
        (precolor, Set.empty)
    | Some (key, vs) -> begin
        match Map.find g key with
        | None ->
            (* If the node with this key was already deleted, we don't
               need to color it. Move on to the remaining nodes. *)
            color g vs ~max ~precolor
        | Some (Vertex.{ adjacent = adj } as v) ->
            (* Update the worklist; some neighbors may become low degree
               after deleting the selected vertex *)
            let init = worklist in
            let w' = update_worklist g adj ~precolor ~max ~init in
            let g' = remove_vertex g v in
            (* Color the subgraph with the selected vertex removed *)
            let m, spills = color g' w' ~max ~precolor in
            (* Afterwards, choose a color for the vertex we removed *)
            let color = choose_color m adj ~max in
            update_coloring m ~key ~spills ~color
      end

  (* The default precoloring: no nodes precolored *)
  let no_precolor (_ : Key.t) = None

  let colors_used m ~max =
    let f ~key:_ ~data acc =
      if data >= max then acc
      else
        match acc with
        | None -> Some data
        | Some color -> Some (Int.max color data)
    in
    match Map.fold m ~init:None ~f with
    | Some max -> succ max
    | None -> 0

  let kempe ?(spills = Set.empty) ?(precolor = no_precolor) g ~max =
    let precolor, w = create_map_worklist g ~spills ~max ~precolor in
    let m, spills = color g w ~max ~precolor in
    if Set.is_empty spills then
      (* No spills, so every node must be assigned a color *)
      Ok (Map.find_exn m, colors_used m ~max)
    else Error spills

  let of_edges =
    List.fold ~init:empty ~f:(fun g (u, v) -> add_edge g u v)
end
