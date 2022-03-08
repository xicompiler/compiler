include module type of Ctx

module Error : module type of CtxError
(** [Error] is the type of an error resulting from a lookup or addition
    to the context *)

(** [Node] represents a node containing a context *)
module Node : sig
  include module type of ContextNode

  module Decorated : module type of DecoratedNode
  (** [Decorated] represents a Node in the decorated AST*)
end
