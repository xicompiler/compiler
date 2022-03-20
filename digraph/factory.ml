open Core

module Make (Key : Abstract.Key) = struct
  module Key = Key

  type key = Key.t

  module H = Hashable.Make_and_derive_hash_fold_t (Key)
  module Table = H.Table

  type ('v, 'e) vertex = {
    key : key;
    value : 'v;
    mutable incoming : ('v, 'e) edge list;
    mutable outgoing : ('v, 'e) edge list;
  }

  and ('v, 'e) edge = {
    src : ('v, 'e) vertex;
    dst : ('v, 'e) vertex;
    weight : 'e;
  }

  module Vertex = struct
    type ('v, 'e) t = ('v, 'e) vertex

    let incoming { incoming } = incoming

    (** [add_incoming ~edge v] pushes [edge] onto the incoming edges of
        [v] *)
    let add_incoming ~edge v = v.incoming <- edge :: v.incoming

    let outgoing { outgoing } = outgoing

    (** [add_outgoing ~edge v] pushes [edge] onto the outgoing edges of
        [v] *)
    let add_outgoing ~edge v = v.outgoing <- edge :: v.outgoing

    let key { key } = key
    let value { value } = value

    let add_edge ~src ~dst ~weight =
      let edge = { src; dst; weight } in
      add_incoming ~edge dst;
      add_outgoing ~edge src

    (** [create ~key ~value] is a fresh vertex with key [key], value
        [value], and no incident edges *)
    let create ~key ~value =
      { key; value; incoming = []; outgoing = [] }
  end

  module Edge = struct
    type ('v, 'e) t = ('v, 'e) edge

    let src { src } = src
    let dst { dst } = dst
    let weight { weight } = weight
  end

  type ('v, 'e) t = ('v, 'e) vertex Table.t

  let create ?size () = Table.create ?size ()

  let set_vertex g ~key ~value =
    let vertex = Vertex.create ~key ~value in
    Table.set g ~key ~data:vertex

  let find_vertex g ~key = Table.find g key
  let find_vertex_exn g ~key = Table.find_exn g key
end
