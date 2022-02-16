open Core

type id = string
(** An [id] is the type of a Xi indentifier *)

(** A [unop] is the type of a Xi unary operator *)
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

(** A [literal] represents a literal char, int, bool, or string value in
    Xi *)
type literal =
  | Int of string
  | Bool of bool
  | Char of Uchar.t
  | String of string

(** An [expr] is the type of a Xi expression *)
type expr =
  | Literal of literal
  | Id of id
  | Array of expr array
  | Bop of binop * expr * expr
  | Uop of unop * expr
  | FnCall of call
  | Index of expr * expr

and call = id * expr list
(** A [call] is the type of a function call represented as a pair
    [(id, args)] where [id] is the name of the function and [args] is
    the list of arguments *)

type typ = expr Type.t
(** A [typ] is a Xi type whose arrays are optionally initialized with an
    expression of type [expr] *)

type decl = id * typ
(** A [decl] is the type of a Xi declaration represented as a pair
    [(id, t)] where [id] is the name of the identifier and [t] is its
    type. *)

type init = decl * expr
(** An [init] is the type of a Xi initialization statement represented
    as a pair [(decl, e)] where [decl] is the declaration of the
    identifier and [e] is the initialization expression. *)

(** An [assign_target] represents the target of an assignment statement
    in Xi; either a variable or an array element. *)
type assign_target =
  | Var of id
  | ArrayElt of assign_target * expr

(** A [multi_target] is the type of a target of a multiple
    initialization expression in Xi; either a declaration or a wildcard,
    [_]. *)
type multi_target =
  | MultiDecl of decl
  | Wildcard

(** A [stmt] is a legal statement in Xi, also known as a command. *)
type stmt =
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Decl of decl
  | Init of init
  | Assign of assign_target * expr
  | MultiInit of multi_target list * call
  | ProcCall of call
  | Return of expr list
  | Block of block

and block = stmt list
(** A [block] is the type of a possible empty block of statements in Xi,
    represented as a list of statements possibly followed by a return
    statement *)

type signature = {
  id : id;
  params : decl list;
  types : typ list;
}
(** A [signature] is a signature or interface for an individual method
    where [types] is the list of (possibly none) return types. *)

type fn = signature * block
(** A [fn] is a Xi function definition whose body is a block of
    statements represented as a pair [(signature, body)] where
    [signature] is the function's signature and [body] is its body. *)

(** A [definition] is the type of a top-level declaration in Xi: either
    a function definition, or declaration or initialization of a global
    variable. *)
type definition =
  | FnDefn of fn
  | GlobalDecl of decl
  | GlobalInit of decl * literal

type source = {
  uses : id list;
  definitions : definition list;
}
(** A [source] describes the structure of a source file in Xi; 0 or more
    use statements followed by 1 or more top-level definitions, at least
    one of which must be a function definition. *)

type interface = signature list
(** An [interface] is a Xi interface, represented as a non-empty list of
    function signatures. *)

(** An expression of type [t] is an expression representing a node of
    the Abstract Syntax Tree of a Xi program, described either by a
    source or interface file. *)
type t =
  | Source of source
  | Interface of interface

val sexp_of_t : t -> Sexp.t
(** [sexp_of_t ast] is the s-expression serialization of [ast]. *)
