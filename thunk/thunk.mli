type 'a t = unit -> 'a
(** ['a t] is a function mapping [unit] to ['a] *)

val map2 : 'a t -> 'a * 'a
(** [map2 f] is [(f (), f ())] *)

val map3 : 'a t -> 'a * 'a * 'a
(** [map3 f] is [(f (), f (), f ())] *)
