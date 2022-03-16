open Core

(** A [S] represents a term in the AST *)
module type S = sig
  type t
  (** [t] is the representation type for this [Node] *)

  val sexp_of_t : t -> Sexp.t
  (** [sexp_of_t node] is the s-expression serialization of [t] *)

  val const_fold : t -> t
  (** [const_fold node] constant folds every expression in [node] *)

  type node
  (** A [node] is a [t] wrapped in a [Node.t] *)

  val sexp_of_node : node -> Sexp.t
  (** [sexp_of_node node] is the s-expression serialization of the value
      wrapped in [node] *)

  val const_fold_node : node -> node
  (** [const_fold_node node] constant folds every value wrapped in
      [node] *)

  val const_fold_nodes : node list -> node list
  (** [const_fold_node nodes] performs a constant fold on every node in
      [nodes] *)
end
