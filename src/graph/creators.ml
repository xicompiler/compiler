open Core

module type S2 = sig
  type ('a, 'b) vertex
  type ('a, 'b) t

  val add_vertex : ('a, 'b) t -> ('a, 'b) vertex -> unit
  val create : ?size:int -> unit -> ('a, 'b) t
  val iter_vertices : ('v, 'e) t -> f:(('v, 'e) vertex -> unit) -> unit
  val of_vertices : ('v, 'e) vertex list -> ('v, 'e) t
end

module type Params2 = sig
  type ('a, 'b) vertex

  module Table : Hashtbl.S

  val key : ('a, 'b) vertex -> Table.key
end

module Make2 (Args : Params2) = struct
  open Args

  type ('a, 'b) t = ('a, 'b) vertex Table.t

  let add_vertex g v = Hashtbl.add_exn g ~key:(key v) ~data:v
  let create ?size () = Table.create ?size ()
  let iter_vertices = Hashtbl.iter
  let of_vertices vs = Table.create_with_key_exn ~get_key:key vs
end

module type S = sig
  type 'a vertex
  type 'a t

  include
    S2
      with type ('a, 'b) vertex := 'a vertex
      with type ('a, 'b) t := 'a t
end

module type Params = sig
  type 'a vertex

  include Params2 with type ('a, 'b) vertex := 'a vertex
end

module Make (Args : Params) = struct
  open Args

  include Make2 (struct
    type ('a, 'b) vertex = 'a Args.vertex

    module Table = Args.Table

    let key = Args.key
  end)

  type nonrec 'a t = ('a, unit) t
end