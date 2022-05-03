open Core

module type Key = Hashtbl.Key

module type S = sig
  module Key : Key

  module Vertex : sig
    include Vertex.S with module Key := Key

    val foldi_adjacent :
      'a t -> init:'acc -> f:(Key.t -> 'acc -> 'a t -> 'acc) -> 'acc

    val fold_adjacent :
      'a t -> init:'acc -> f:('acc -> 'a t -> 'acc) -> 'acc

    val iter_adjacent : 'a t -> f:('a t -> unit) -> unit
    val add_edge : 'a t -> 'a t -> unit
  end

  type 'a t

  val create : ?size:int -> unit -> 'a t
end

module Make (Key : Key) = struct
  module Key = Key
  module Table = Hashtbl.Make (Key)

  module Vertex = Vertex.Make (struct
    module Key = Key

    type 'a t = {
      key : Key.t;
      mutable value : 'a;
      mutable marked : bool;
      adjacent : 'a t Table.t;
    }
    [@@deriving fields]

    let set v ~value = v.value <- value

    let foldi_adjacent v ~init ~f =
      let f ~key ~data acc = f key acc data in
      Hashtbl.fold v.adjacent ~init ~f

    let fold_adjacent v ~init ~f =
      foldi_adjacent v ~init ~f:(fun _ -> f)

    let iter_adjacent v ~f = fold_adjacent v ~init:() ~f:(fun _ -> f)
    let mark v = v.marked <- true

    (** [add_adjacent u v] adds [v] to the adjacent vertices of [u] *)
    let add_adjacent u v = Hashtbl.add_exn u.adjacent ~key:v.key ~data:v

    let add_edge u v =
      add_adjacent u v;
      add_adjacent v u

    let create ~key ~value =
      { key; value; marked = false; adjacent = Table.create () }
  end)

  type 'a t = 'a Vertex.t Table.t

  let create ?size () = Table.create ?size ()
end

module Vertex = Vertex
