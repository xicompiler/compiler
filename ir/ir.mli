open Core

(** [Mir] is a mid level intermediate representation *)
module Mir : module type of struct
  include Mir
end

(** [Lir] is a low level intermediate representation *)
module Lir : module type of struct
  include Lir
end

(** [Subtype] is the subtype of an IR node *)
module Subtype : module type of struct
  include Subtype
end

(** [Reorder] contains functions for reordering lowered IR *)
module Reorder : module type of struct
  include Reorder
end

val translate : Ast.Decorated.Toplevel.source -> Reorder.t
(** [translate ast] is decorated ast [ast] translated to lowered
    (canonical) IR *)

val sexp_of_t : compunit:string -> Reorder.t -> Sexp.t
(** [sexp_of_t ~compunit tlist] is the s-expression representation of
    the reordered toplevel list with COMPUNIT name [compunit] *)

val const_fold : Reorder.t -> Reorder.t
(** [const_fold stmts] is [stmts] constant folded at the IR level *)

val translate :
  optimize:bool -> Ast.Decorated.Toplevel.source -> Reorder.t
(** [translate ~optimize ast] is decorated ast [ast] translated to
    lowered (canonical) IR, with optimizations if [optimize] is true *)

open Frontend

module Diagnostic : sig
  val file_to_file :
    ?cache:Check.cache ->
    src:string ->
    out:string ->
    deps:Check.dependencies ->
    optimize:bool ->
    unit ->
    unit File.Xi.result
  (** [file_to_file ~start lexbuf out] parses and typechecks the
      contents of file [src] and generates ir to file [out] *)
end
