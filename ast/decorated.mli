open Core

(** [Ex] is a module wrapping an expression node *)
module Ex :
  Node.Context with type typ = Type.expr and type context = Type.context

(** [St] is a module wrapping a statement node *)
module St : sig
  include
    Node.Context
      with type typ = Type.stmt
       and type context = Type.Context.fn

  val make_unit : 'a -> ctx:context -> 'a t
  (** [make_unit v ~ctx] is [make v ~ctx ~typ:`Unit] *)
end

include
  Abstract.S with module Expr.Node := Ex and module Stmt.Node := St

type nonrec result = (t, Type.error Position.error) result
(** [result] is either [Ok ast] or [Error err], where [err] describes
    the position and cause of the semantic error. *)
