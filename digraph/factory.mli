open Core
open Abstract

(** [Make (Key)] is a digraph with [Key] as the key *)
module Make (Key : Key) : S with module Key = Key
