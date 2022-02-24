open Core

module type S = sig
  include Types.S

  type id = string

  module Expr : sig
    type equality =
      [ `Eq
      | `Neq
      ]

    type logical =
      [ `And
      | `Or
      ]

    type compare =
      [ `Leq
      | `Lt
      | `Gt
      | `Geq
      ]

    type arith =
      [ `Mult
      | `HighMult
      | `Div
      | `Mod
      | `Plus
      | `Minus
      ]

    type binop =
      [ equality
      | logical
      | compare
      | arith
      ]

    type _ literal =
      | Int : string -> integer literal
      | Bool : bool -> boolean literal
      | Char : Uchar.t -> integer literal
      | String : string -> integer vector literal

    module Node : Node.S

    type _ t =
      | Literal : 'a literal -> 'a t
      | Id : id -> 'a t
      | Array : 'a node array -> 'a vector t
      | Equality : equality * 'a node * 'a node -> boolean t
      | Logical : logical * boolean node * boolean node -> boolean t
      | Compare : compare * integer node * integer node -> boolean t
      | IntNeg : integer node -> integer t
      | LogicalNeg : boolean node -> boolean t
      | Arith : arith * integer node * integer node -> integer t
      | FnCall : call -> 'a t
      | Index : 'a vector node * integer node -> 'a t

    and 'a node = 'a t Node.t
    and call = id * wrap list
    and wrap = Wrap : _ node -> wrap

    val return : 'a node -> wrap
  end

  type 'a expr = 'a Expr.t

  module Type : sig
    type nonrec primitive =
      | Int
      | Bool

    type t =
      | Primitive of primitive
      | Array of t * integer Expr.node option

    val array : t -> integer Expr.node option -> t
  end

  module Stmt : sig
    type decl = id * Type.t
    type 'a init = decl * 'a Expr.node

    type assign_target =
      | Var of id
      | ArrayElt of assign_target * integer Expr.node

    type multi_target =
      | MultiDecl of decl
      | Wildcard

    module Node : Node.S

    type t =
      | If of boolean Expr.node * node * node option
      | While of boolean Expr.node * node
      | Decl of decl
      | Init : 'a init -> t
      | Assign : assign_target * 'a Expr.node -> t
      | MultiInit of multi_target list * Expr.call
      | ProcCall of Expr.call
      | Return of Expr.wrap list
      | ExprStmt : 'a Expr.node -> t
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
    | GlobalInit : Stmt.decl * 'a Expr.literal -> definition

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
