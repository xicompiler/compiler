open Core

module Ex = struct
  type typ = Type.expr
  type context = Type.context
  type nonrec 'a result = ('a, Type.error Position.error) result

  type 'a t = {
    expr : 'a;
    context : context;
    typ : typ;
  }

  let context { context } = context
  let typ { typ } = typ
  let get { expr } = expr
  let make expr ~ctx ~typ = { expr; context = ctx; typ }
end

module St = struct
  type typ = Type.stmt
  type context = Type.Context.fn
  type nonrec 'a result = ('a, Type.error Position.error) result

  type 'a t = {
    stmt : 'a;
    context : context;
    typ : typ;
  }

  let context { context } = context
  let typ { typ } = typ
  let get { stmt } = stmt
  let make stmt ~ctx ~typ = { stmt; context = ctx; typ }
  let make_unit = make ~typ:`Unit
end

include Factory.Make (Ex) (St)

type nonrec result = (t, Type.error Position.error) result
