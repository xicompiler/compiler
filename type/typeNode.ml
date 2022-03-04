open Definitions

module type Params = sig
  type typ
  type context
end

module type S = sig
  include Node.S
  include Params

  val context : 'a t -> context
  val typ : 'a t -> typ
  val make : 'a -> ctx:context -> typ:typ -> pos:Position.t -> 'a t
  val position : 'a t -> Position.t

  val positioned :
    error:TypeError.error -> 'a t -> TypeError.Positioned.error
end

(** [Make (Args)] is a concrete Node with concrete types wrapped in
    [Args] *)
module Make (Args : Params) = struct
  include Args

  type 'a t = {
    value : 'a;
    context : context;
    typ : typ;
    position : Position.t;
  }

  let context { context } = context
  let typ { typ } = typ
  let get { value } = value

  let make value ~ctx ~typ ~pos =
    { value; context = ctx; typ; position = pos }

  let position { position } = position

  let positioned ~error { position } =
    TypeError.Positioned.make ~pos:position error
end

module Expr = Make (struct
  type typ = expr
  type context = Context.context
end)

type 'a expr = 'a Expr.t

module Stmt = struct
  include Make (struct
    type typ = stmt
    type context = Context.fn
  end)

  let make_unit = make ~typ:`Unit
  let make_void = make ~typ:`Void
end

type 'a stmt = 'a Stmt.t
