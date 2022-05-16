open Core

module type S = sig
  type key
  type t

  val create : unit -> t
  val find : t -> key -> key
  val union : t -> key -> key -> unit
end

module Make (Key : Hashtbl.Key) = struct
  module Table = Hashtbl.Make (Key)

  type node = {
    mutable parent : node option;
    mutable rank : int;
    key : Key.t;
  }

  type t = node Table.t

  let create () = Table.create ()

  let rec find_root node =
    match node.parent with
    | None -> node
    | Some parent ->
        let root = find_root parent in
        node.parent <- Some root;
        root

  let create_node key = { parent = None; rank = 0; key }

  let find_root set key =
    find_root (Hashtbl.findi_or_add set key ~default:create_node)

  let find set key = (find_root set key).key

  let union_roots n1 n2 =
    let rank1 = n1.rank in
    let rank2 = n2.rank in
    if rank1 < rank2 then n1.parent <- Some n2
    else (
      n2.parent <- Some n1;
      if rank1 = rank2 then n1.rank <- succ rank1)

  let union set k1 k2 =
    let root1 = find_root set k1 in
    let root2 = find_root set k2 in
    if Key.compare root1.key root2.key <> 0 then union_roots root1 root2
end
