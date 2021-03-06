open Core
include Abstract
include Factory

module IntDigraph = struct
  include Make (Int)

  let unused_key g = match max_key g with Some k -> succ k | None -> 0

  let indexed =
    List.mapi ~f:(fun key value -> Vertex.create ~key ~value)
end
