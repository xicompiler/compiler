open! Core

val ( >> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** [(g >> f) x] is [f (g x)] *)

val ( << ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** [(f << g) x] is [f (g x)] *)
