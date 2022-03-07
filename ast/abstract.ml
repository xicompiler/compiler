open Core

type id = string Node.Position.t
type decl = id * Type.tau

type signature = {
  id : id;
  params : decl list;
  types : Type.tau list;
}

module type S = sig
  module Expr : sig
    include module type of Op
    include module type of Primitive
    module Node : Node.S

    type t =
      | Primitive of primitive
      | Id of id
      | Array of node array
      | String of string
      | Bop of binop * node * node
      | Uop of unop * node
      | FnCall of call
      | Length of node
      | Index of index

    and node = t Node.t
    and nodes = node list
    and call = id * nodes
    and index = node * node
  end

  type expr = Expr.t

  module Stmt : sig
    module Node : Node.S

    type t =
      | If of Expr.node * node
      | IfElse of Expr.node * node * node
      | While of Expr.node * node
      | VarDecl of decl
      | ArrayDecl of id * Type.tau * Expr.node option list
      | Assign of id * Expr.node
      | ArrAssign of Expr.node * Expr.node * Expr.node
      | ExprStmt of Expr.call
      | VarInit of id * Type.tau * Expr.node
      | MultiAssign of decl option list * id * Expr.nodes
      | PrCall of Expr.call
      | Return of Expr.nodes
      | Block of block

    and node = t Node.t
    and block = node list
  end

  type stmt = Stmt.t

  module Toplevel : sig
    module Node : Node.S

    type fn = signature * Stmt.block

    type definition =
      | FnDefn of fn
      | GlobalDecl of decl
      | GlobalInit of id * Type.tau * Expr.primitive

    type node = definition Node.t

    type source = {
      uses : id Node.t list;
      definitions : node list;
    }

    type intf = signature Node.t list
  end

  type t =
    | Source of Toplevel.source
    | Intf of Toplevel.intf
  [@@deriving variants]

  val sexp_of_t : t -> Sexp.t
end
