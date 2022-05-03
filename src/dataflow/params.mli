open! Core

type direction =
  [ `Forward
  | `Backward
  ]
(** [direction] is the direction in a datalow analysis *)

type 'data meet = 'data list -> 'data
(** ['data meet] is the type of a meet function that takes the meet of
    values of type ['data] *)

type ('data, 'v) t = {
  f : data:'data -> vertex:'v -> 'data;
  meet : 'data meet;
  top : 'data;
  direction : direction;
  equal : 'data -> 'data -> bool;
}
(** A [('data, 'v) t] is an instance of dataflow analysis parameters
    parameterized over dataflow values of type ['data] and nodes
    carrying values of type ['v] *)
