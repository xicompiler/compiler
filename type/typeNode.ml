open Definitions
open TypeError

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
end

type 'a stmt = 'a Stmt.t
