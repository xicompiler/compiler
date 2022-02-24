open Core

(** [S] is the signature of a generic AST *)
module type S = sig
  include Types.S

  type id = string
  (** An [id] is the type of a Xi indentifier *)

  (** An [Expr] represents an expression in the Xi langauge *)
  module Expr : sig
    type equality =
      [ `Eq
      | `Neq
      ]
    (** [equality] is the type of a equality comparison operator *)

    type logical =
      [ `And
      | `Or
      ]
    (** [logical] is the type of a binary operator applied to booleans *)

    type compare =
      [ `Leq
      | `Lt
      | `Gt
      | `Geq
      ]
    (** [compare] is the type of an binary comparison operator applied
        to integers *)

    type arith =
      [ `Mult
      | `HighMult
      | `Div
      | `Mod
      | `Plus
      | `Minus
      ]
    (** [arith] is the type of an arithmetic binary operator applied to
        integers *)

    type binop =
      [ equality
      | logical
      | compare
      | arith
      ]
    (** A [binop] is a binary operator *)

    (** A [literal] represents a literal char, int, bool, or string
        value in Xi *)
    type _ literal =
      | Int : string -> integer literal
      | Bool : bool -> boolean literal
      | Char : Uchar.t -> integer literal
      | String : string -> integer vector literal

    module Node : Node.S
    (** [Node] wraps an expression node *)

    (** An [t] is the type of a Xi expression *)
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
    (** [node] is the type of an expression node *)

    and call = id * wrap list
    (** A [call] is the type of a function call represented as a pair
        [(id, args)] where [id] is the name of the function and [args]
        is the list of arguments *)

    (** A [wrap] is an existentially wrapped expression node *)
    and wrap = Wrap : _ node -> wrap

    val return : 'a node -> wrap
    (** [return e] is [Wrap e] *)
  end

  type 'a expr = 'a Expr.t
  (** An [expr] is a Xi expression *)

  (** [Type] is the type of an expression in the AST *)
  module Type : sig
    (** A [primitive] is the type of a primitive value in Xi: either an
        integer or a boolean *)
    type nonrec primitive =
      | Int
      | Bool

    (** A type in Xi is either a primitive type or an array of a type,
        where an Array is represented by a pair (contents, length) *)
    type t =
      | Primitive of primitive
      | Array of t * integer Expr.node option

    val array : t -> integer Expr.node option -> t
    (** [array t length] is an [Array] type with contents of type [t]
        and optional length [length] *)
  end

  module Stmt : sig
    (** A [typ] is a Xi type whose arrays are optionally initialized
        with an expression of type [expr] *)
    type decl = id * Type.t
    (** A [decl] is the type of a Xi declaration represented as a pair
        [(id, t)] where [id] is the name of the identifier and [t] is
        its type. *)

    type 'a init = decl * 'a Expr.node
    (** An [init] is the type of a Xi initialization statement
        represented as a pair [(decl, e)] where [decl] is the
        declaration of the identifier and [e] is the initialization
        expression. *)

    (** An [assign_target] represents the target of an assignment
        statement in Xi; either a variable or an array element. *)
    type assign_target =
      | Var of id
      | ArrayElt of assign_target * integer Expr.node

    (** A [multi_target] is the type of a target of a multiple
        initialization expression in Xi; either a declaration or a
        wildcard, [_]. *)
    type multi_target =
      | MultiDecl of decl
      | Wildcard

    module Node : Node.S
    (** A [Node] is a statement node in the ast *)

    (** A [stmt] is a legal statement in Xi, also known as a command. *)
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
    types : Type.t list;
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
    | GlobalInit : Stmt.decl * 'a Expr.literal -> definition

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
