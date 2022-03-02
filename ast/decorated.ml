open Core

module type ContextNode = sig
  include Node.S

  type typ
  type context

  val context : 'a t -> context
  val typ : 'a t -> typ
  val make : 'a -> ctx:context -> typ:typ -> 'a t
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
    context : Type.Context.fn;
    typ : Type.stmt;
  }

  let context { context } = context
  let typ { typ } = typ
  let get { stmt } = stmt
  let make stmt ~ctx ~typ = { stmt; context = ctx; typ }
end

include Factory.Make (Ex) (St)

type nonrec result = t Type.result
