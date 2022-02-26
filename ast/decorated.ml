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
end

module Context = Map.Make (String)

type context = Type.env Context.t

module type ContextNode = sig
  include Node.S

  val context : 'a t -> context
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
end

include Factory.Make (Ex) (St)
