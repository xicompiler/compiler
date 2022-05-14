open Core

module type Key = Hashtbl.Key

module type S = sig
  module Key : Key

  type ('v, 'e) vertex
  type ('v, 'e) edge

  module Vertex : sig
    type ('v, 'e) t = ('v, 'e) vertex

    val create : key:Key.t -> value:'v -> ('v, 'e) t
    val incoming : ('v, 'e) t -> ('v, 'e) edge list
    val pred : ('v, 'e) t -> ('v, 'e) t list
    val outgoing : ('v, 'e) t -> ('v, 'e) edge list
    val succ : ('v, 'e) t -> ('v, 'e) t list
    val has_succ : ('v, 'e) t -> target:('v, 'e) t -> bool
    val peek_succ : ('v, 'e) t -> ('v, 'e) t option
    val key : ('v, 'e) t -> Key.t
    val value : ('v, 'e) t -> 'v
    val set : ('v, 'e) t -> value:'v -> unit
    val update : ('v, 'e) t -> f:('v -> 'v) -> unit
    val fold : ('v, 'e) t -> f:('v -> 'a) -> 'a
    val marked : ('v, 'e) t -> bool
    val unmarked : ('v, 'e) t -> bool
    val mark : ('v, 'e) t -> unit
    val has_unmarked_pred : ('v, 'e) t -> bool

    val add_unweighted_edge :
      src:('v, unit) t -> dst:('v, unit) t -> unit

    val add_edge : src:('v, 'e) t -> dst:('v, 'e) t -> weight:'e -> unit
    val set_edge : src:('v, 'e) t -> dst:('v, 'e) t -> weight:'e -> unit
    val remove_edge : src:('v, 'e) t -> dst:('v, 'e) t -> unit

    val exists_incoming :
      ('v, 'e) t -> f:(('v, 'e) edge -> bool) -> bool

    val equal : ('v, 'e) t -> ('v, 'e) t -> bool
  end

  module Edge : sig
    type ('v, 'e) t = ('v, 'e) edge

    val src : ('v, 'e) t -> ('v, 'e) vertex
    val dst : ('v, 'e) t -> ('v, 'e) vertex
    val weight : ('v, 'e) t -> 'e
  end

  type ('v, 'e) t

  val create : ?size:int -> unit -> ('v, 'e) t
  val of_vertices : ('v, 'e) vertex list -> ('v, 'e) t
  val max_key : ('v, 'e) t -> Key.t option

  val insert_after :
    ('v, 'e) t -> ('v, 'e) vertex -> prev:('v, 'e) vertex -> unit

  val insert_before :
    ('v, 'e) t -> ('v, 'e) vertex -> next:('v, 'e) vertex -> unit

  val foldi_vertices :
    ('v, 'e) t ->
    init:'acc ->
    f:(Key.t -> 'acc -> ('v, 'e) vertex -> 'acc) ->
    'acc

  val iteri_vertices :
    ('v, 'e) t -> f:(Key.t -> ('v, 'e) vertex -> unit) -> unit

  val iter_vertices : ('v, 'e) t -> f:(('v, 'e) vertex -> unit) -> unit

  val analyze :
    ('v, 'e) t ->
    ('data, 'v) Dataflow.Params.t ->
    Key.t ->
    'data Dataflow.Values.t

  val graphviz :
    string_of_vertex:(('v, 'e) vertex -> string) ->
    string_of_weight:('e -> string) ->
    ('v, 'e) vertex list ->
    string
end