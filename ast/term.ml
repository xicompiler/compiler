open Core

module type S = sig
  type t

  val sexp_of_t : t -> Sexp.t
  val const_fold : t -> t

  type node

  val sexp_of_node : node -> Sexp.t
  val const_fold_node : node -> node
  val const_fold_nodes : node list -> node list
end
