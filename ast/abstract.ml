open Core

module type S = sig
  type id = string

  module Expr : sig
    type unop =
      | IntNeg
      | LogicalNeg

    type binop =
      | Mult
      | HighMult
      | Div
      | Mod
      | Plus
      | Minus
      | Lt
      | Leq
      | Geq
      | Gt
      | Eq
      | Neq
      | And
      | Or

    type primitive =
      | Int of string
      | Bool of bool
      | Char of Uchar.t

    module Node : Node.S

    type t =
      | Primitive of primitive
      | Id of id
      | Array of node array
      | String of string
      | Bop of binop * node * node
      | Uop of unop * node
      | FnCall of call
      | Index of node * node

    and node = t Node.t
    and call = id * node list
  end

  type expr = Expr.t

  module Type : sig
    module N : Node.S with type 'a t = 'a * Expr.node option
    include Type.S with module Node = N

    val array : t -> Expr.node option -> t
  end

  module Stmt : sig
    type decl = id * Type.t

    type assign_target =
      | Var of id
      | ArrayElt of assign_target * Expr.node

    type init_target =
      | InitDecl of decl
      | Wildcard

    type init = init_target * Expr.node

    module Node : Node.S

    type t =
      | If of Expr.node * node * node option
      | While of Expr.node * node
      | Decl of decl
      | Init of init
      | Assign of assign_target * Expr.node
      | MultiInit of init_target list * Expr.call
      | PrCall of Expr.call
      | Return of expr list
      | Block of block

    and node = t Node.t
    and block = node list
  end

  type stmt = Stmt.t

  type signature = {
    id : id;
    params : Stmt.decl list;
    types : Type.t list;
  }

  type fn = signature * Stmt.block

  type definition =
    | FnDefn of fn
    | GlobalDecl of Stmt.decl
    | GlobalInit of Stmt.decl * Expr.primitive

  type source = {
    uses : id list;
    definitions : definition list;
  }

  type interface = signature list

  type t =
    | Source of source
    | Interface of interface

  val sexp_of_t : t -> Sexp.t
end
