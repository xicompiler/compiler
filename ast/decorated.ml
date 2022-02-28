open Core

module Type = struct
  type tau = Tau.t

  type expr =
    [ tau
    | `Tuple of tau list
    ]

  type kind =
    [ expr
    | `Unit
    ]

  type stmt =
    [ `Unit
    | `Void
    ]

  type env =
    [ `Var of tau
    | `Ret of kind
    | `Fn of kind * kind
    ]

  type error = |
  type nonrec 'a result = ('a, error) result
end

module Context = Map.Make (String)

type context = Type.env Context.t

module type ContextNode = sig
  include Node.S

  type typ

  val context : 'a t -> context
  val typ : 'a t -> typ
  val make : 'a -> ctx:context -> typ:typ -> 'a t
end

module Ex = struct
  type 'a t = {
    expr : 'a;
    context : context;
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
    context : context;
    typ : Type.stmt;
  }

  let context { context } = context
  let typ { typ } = typ
  let get { stmt } = stmt
  let make stmt ~ctx ~typ = { stmt; context = ctx; typ }
end

include Factory.Make (Ex) (St)

type nonrec result = t Type.result
