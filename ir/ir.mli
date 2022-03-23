open Core

(** [Mir] is a mid level intermediate representation *)
module Mir : module type of struct
  include Mir
end

(** [Lir] is a low level intermediate representation *)
module Lir : module type of struct
  include Lir
end

(** [Reorder] contains functions for reordering lowered IR *)
module Reorder : module type of struct
  include Reorder
end

val translate : Ast.Decorated.t -> Reorder.t
(** [translate ast] is decorated ast [ast] translated to lowered
    (canonical) IR *)

val const_fold : Reorder.t -> Reorder.t
(** [const_fold stmts] is [stmts] constant folded at the IR level *)

val sexp_of_t : string -> Reorder.t -> Sexp.t