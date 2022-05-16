open Core

module type S = sig
  type key
  type t

  val create : unit -> t
  val find : t -> key -> key
  val union : t -> key -> key -> unit
end

module Make (Key : Hashtbl.Key) : S with type key := Key.t
