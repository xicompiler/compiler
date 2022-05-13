open Core

type 'data t = {
  input : 'data;
  output : 'data;
}
(** ['data values] represent the dataflow values computed at each node,
    i.e. the dataflow values that are the input and output to the
    transfer function *)

val input : 'data t -> 'data
(** [input values] is [values.input] *)

val output : 'data t -> 'data
(** [output values] is [values.output] *)
