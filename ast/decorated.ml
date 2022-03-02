open Core

module type ContextNode = sig
  include Node.S

  type typ

  val context : 'a t -> Type.context
  val typ : 'a t -> typ
  val make : 'a -> ctx:Type.context -> typ:typ -> 'a t
end

module Ex = struct
  type 'a t = {
    expr : 'a;
    context : Type.context;
    typ : Type.expr;
  }

  let context { context } = context
  let typ { typ } = typ
  let get { expr } = expr
  let make expr ~ctx ~typ = { expr; context = ctx; typ }
end

module St = struct
  type 'a t = {
    stmt : 'a;
    context : Type.context;
    typ : Type.stmt;
  }

  let context { context } = context
  let typ { typ } = typ
  let get { stmt } = stmt
  let make stmt ~ctx ~typ = { stmt; context = ctx; typ }
end

include Factory.Make (Ex) (St)

type nonrec result = t Type.result
