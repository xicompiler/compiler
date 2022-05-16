open Core

val sexp_of_t : compunit:string -> Reorder.t -> Sexp.t
(** [sexp_of_t ~compunit tlist] is the s-expression representation of
    the reordered toplevel list with COMPUNIT name [compunit] *)

val translate :
  opt:Opt.t -> ?gensym:IrGensym.t -> Ast.Decorated.source -> Reorder.t
(** [translate ~opt ?gensym ast] is decorated ast [ast] translated to
    lowered (canonical) IR, with optimizations [opt] *)

(** [Output] represents the file functions needed for IR translation *)
module Output : sig
  val iter_source :
    src:string ->
    ok:(Ast.Decorated.source -> unit) ->
    ?err:(Frontend.Check.error -> unit) ->
    ?cache:Frontend.Check.cache ->
    deps:Frontend.Check.dependencies ->
    unit ->
    Frontend.Check.result File.Xi.result
  (** [iter_source ?cache ~src ~deps ~f ()] applies f to the decorated
      AST source constructed from the file at [src] if possible
      resolving dependentcies from [deps], or is [()] if this is not
      possible. *)

  val file_to_file :
    ?cache:Frontend.Check.cache ->
    src:string ->
    out:string ->
    deps:Frontend.Check.dependencies ->
    opt:Opt.t ->
    unit ->
    Frontend.Check.result File.Xi.result
  (** [file_to_file ~start lexbuf out] parses and typechecks the
      contents of file [src] and generates ir to file [out] *)
end

(** [Mir] is a mid level intermediate representation *)
module Mir : module type of struct
  include Mir
end

(** [Lir] is a low level intermediate representation *)
module Lir : module type of struct
  include Lir
end

include module type of struct
  include Subtype
end

(** [Reorder] contains functions for reordering lowered IR *)
module Reorder : module type of struct
  include Reorder
end

(** [Op] represents an Op in Ir *)
module Op : module type of struct
  include Op
end

(** [Gensym] is the type of a symbol generator for IR terms *)
module Gensym : module type of struct
  include IrGensym
end

(** [Temp] represents a temporary in IR *)
module Temp : module type of struct
  include Temp
end

(** [ConstFold] contains functions for constant folding IR *)
module ConstFold : module type of struct
  include ConstFold
end

(** [CopyProp] contains functions for copy propagation in IR *)
module CopyProp : module type of struct
  include CopyProp
end
