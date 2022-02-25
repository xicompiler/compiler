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

    type literal =
      | Int of string
      | Bool of bool
      | Char of Uchar.t
      | String of string

    module Node : Node.S

    type t =
      | Literal of literal
      | Id of id
      | Array of node array
      | Bop of binop * node * node
      | Uop of unop * node
      | FnCall of call
      | Index of node * node

    and node = t Node.t
    and call = id * node list
  end

  type expr = Expr.t

  module Type : sig
    type nonrec primitive =
      | Int
      | Bool

    type t =
      | Primitive of primitive
      | Array of t * Expr.node option

    val array : t -> Expr.node option -> t
  end

  module Stmt : sig
    type decl = id * Type.t
    type init = decl * Expr.node

    type assign_target =
      | Var of id
      | ArrayElt of assign_target * Expr.node

    type multi_target =
      | MultiDecl of decl
      | Wildcard

    module Node : Node.S

    type t =
      | If of Expr.node * node * node option
      | While of Expr.node * node
      | Decl of decl
      | Init of init
      | Assign of assign_target * Expr.node
      | MultiInit of multi_target list * Expr.call
      | ProcCall of Expr.call
      | Return of Expr.node list
      | ExprStmt of Expr.node
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
    | GlobalInit of Stmt.decl * Expr.literal

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
