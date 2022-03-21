open Core

type 'a t = {
  mutable counter : int;
  format : (int -> 'a, unit, string, string, string, string) format6;
}
(** [t] is the type of a symbol generator *)

let create ?(init = 0) format = { counter = init; format }

let generate gen =
  let symbol = Printf.sprintf gen.format gen.counter in
  gen.counter <- succ gen.counter;
  symbol
