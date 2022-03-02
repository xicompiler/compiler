open Core

(** [S] is the signature of a generic AST *)
module type S = sig
  type id = string
  (** An [id] is the type of a Xi identifier *)

  (** An [Expr] represents an expression in the Xi langauge *)
  module Expr : sig
    type unop =
      | IntNeg
      | LogicalNeg

    (** A [binop] is the type of a Xi binary operator *)
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

    (** A [literal] represents a literal char, int, bool, or string
        value in Xi *)
    type primitive =
      | Int of string
      | Bool of bool
      | Char of Uchar.t

    module Node : Node.S
    (** [Node] wraps an expression node *)

    (** [t] is the type of an expression in the AST *)
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
    (** [node] is the type of an expression node *)

    and nodes = node list
    (** [nodes] is the type of a list of expression nodes *)

    and call = id * nodes
    (** A [call] is the type of a function call represented as a pair
        [(id, args)] where [id] is the name of the function and [args]
        is the list of arguments *)

    and index = node * node
    (** [index] represents an index expression [e1\[e2\]] as a pair
        [(e1, e2)]*)
  end

  type expr = Expr.t
  (** An [expr] is a Xi expression *)

  module Stmt : sig
    (** A [typ] is a Xi type whose arrays are optionally initialized
        with an expression of type [expr] *)

    type decl = id * Type.tau
    (** A [decl] is the type of a Xi declaration represented as a pair
        [(id, t)] where [id] is the name of the identifier and [t] is
        its type. *)

    module Node : Node.S
    (** A [Node] is a statement node in the ast *)

    (** A [stmt] is a legal statement in Xi, also known as a command. *)
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
    (** [node] is the type of a statement node *)

    and block = node list
    (** A [block] is the type of a possible empty block of statements in
        Xi, represented as a list of statements possibly followed by a
        return statement *)
  end

  type stmt = Stmt.t
  (** [stmt] is an alias for [Stmt.t] *)

  type signature = {
    id : id;
    params : Stmt.decl list;
    types : Type.tau list;
  }
  (** A [signature] is a signature or interface for an individual method
      where [types] is the list of (possibly none) return types. *)

  type fn = signature * Stmt.block
  (** A [fn] is a Xi function definition whose body is a block of
      statements represented as a pair [(signature, body)] where
      [signature] is the function's signature and [body] is its body. *)

  (** A [definition] is the type of a top-level declaration in Xi:
      either a function definition, or declaration or initialization of
      a global variable. *)
  type definition =
    | FnDefn of fn
    | GlobalDecl of Stmt.decl
    | GlobalInit of id * Type.tau * Expr.primitive

  type source = {
    uses : id list;
    definitions : definition list;
  }
  (** A [source] describes the structure of a source file in Xi; 0 or
      more use statements followed by 1 or more top-level definitions,
      at least one of which must be a function definition. *)

  type interface = signature list
  (** An [interface] is a Xi interface, represented as a non-empty list
      of function signatures. *)

  (** An expression of type [t] is an expression representing a node of
      the Abstract Syntax Tree of a Xi program, described either by a
      source or interface file. *)
  type t =
    | Source of source
    | Interface of interface

  val sexp_of_t : t -> Sexp.t
  (** [sexp_of_t ast] is the s-expression serialization of [ast]. *)
end
