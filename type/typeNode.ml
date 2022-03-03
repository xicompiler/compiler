open Definitions
open TypeError

module type S = sig
  include Node.S

  type typ
  type context
  type nonrec 'a result = ('a, error Position.error) Result.t

  val context : 'a t -> context
  val typ : 'a t -> typ
  val make : 'a -> ctx:context -> typ:typ -> 'a t
end

module Expr = struct
  type typ = expr
  type context = Context.context
  type nonrec 'a result = ('a, error Position.error) Result.t

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

module Stmt = struct
  type typ = stmt
  type context = Context.fn
  type nonrec 'a result = ('a, error Position.error) Result.t

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