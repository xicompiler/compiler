open Core

(** [Params] represent the parameters that can be used to construct a
    [Vertex] *)
module type Params2 = sig
  (** [Key] represents the unique key of a vertex *)
  module Key : sig
    type t [@@deriving compare]
  end

  type ('a, 'b) t
  (** A [('a, 'b) t] is a vertex carrying a value of type ['a] and
      adjacent to edges carrying type ['b] *)

  val create : key:Key.t -> value:'a -> ('a, 'b) t
  (** [create ~key ~value] is a fresh unmarked vertex with key [key],
      value [value] and no incident edges *)

  val key : ('a, 'b) t -> Key.t
  (** [key v] is the unique key of [v] *)

  val set : ('v, 'e) t -> value:'v -> unit
  (** [set_value v ~value] sets the value of vertex [v] to [value] *)

  val value : ('a, 'b) t -> 'a
  (** [value vertex] is the value stored by [vertex] *)

  val marked : ('v, 'e) t -> bool
  (** [marked vertex] is [true] iff [vertex] has been marked *)

  val mark : ('v, 'e) t -> unit
  (** [mark vertex] marks [vertex] *)
end

(** [S2] represents a vertex that carries a value and whose incident
    edges also carry a weight *)
module type S2 = sig
  include Params2

  val fold : ('v, 'e) t -> f:('v -> 'a) -> 'a
  (** [fold vertex ~f] is [f (value vertex)] *)

  val update : ('v, 'e) t -> f:('v -> 'v) -> unit
  (** [update v ~f] applies [f] to the value of [g] and writes the
      computed result back to [v] *)

  val unmarked : ('v, 'e) t -> bool
  (** [unmarked vertex] is [true] iff [vertex] is unmarked *)

  val equal : ('v, 'e) t -> ('v, 'e) t -> bool
  (** [equal v1 v2] is [true] iff their keys are equivalent *)
end

(** [Make (Args)] is an [S2] with the same representation type and [Key]
    as [Args] *)
module Make2 (Args : Params2) :
  S2 with type ('a, 'b) t = ('a, 'b) Args.t with module Key = Args.Key

(** [S] represents a vertex with unweighted edges *)
module type S = sig
  type 'a t
  (** ['a t] is the type of a vertex with value of type ['a] *)

  include S2 with type ('a, 'b) t := 'a t
end

(** [Params] is the module type that can be used to construct an [S] *)
module type Params = sig
  type 'a t
  (** ['a t] is the type of a vertex wrapping values of type ['a] *)

  include Params2 with type ('a, 'b) t := 'a t
end

(** [Make (Args)] is an [S] with its representation type and [Key]
    module equivalent to that of [Args]*)
module Make (Args : Params) :
  S with type 'a t = 'a Args.t with module Key = Args.Key
