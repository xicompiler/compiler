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

(** An [expr] is the type of a Xi expression *)
type expr =
  | Id of string
  | Int of int64
  | Bool of bool
  | Array of expr array
  | Bop of binop * expr * expr
  | Uop of unop * expr
  | App of id * expr list
  | Index of expr * expr

(** A [Var] is a variable in the AST *)
module Var : sig
  type decl = id * Type.t
  (** A [decl] is the declaration of a typed, named identifier
      represented as a pair [(id, type)] where [id] is the name of the
      identifier and [type] is its type. *)

  (** An [assignee] is an expression that can be assigned to *)
  type assignee =
    | Existing of id
    | New of decl
    | Wildcard

  type assign = assignee list * expr list
  (** A [assign] is the type of a variable assignment statement,
      represented by a pair [(lhs, rhs)] where [lhs] is the list of
      assignees. [rhs] comprises the right hand side of the statement. *)

  (** A [stmt] involving a variable is either an assignment or
      declaration. *)
  type stmt =
    | Assign of assign
    | Decl of decl list
end

(** A [stmt] is a legal statement in Xi, also known as a command. *)
type stmt =
  | Block of stmt list
  | Var of Var.stmt
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Return of expr list
  | Proc of id * expr list

type signature = {
  id : id;
  args : Var.decl list;
  types : Type.t list;
}
(** A [signature] is a signature or interface for an individual method. *)

type fn = signature * stmt list
(** A [fn] is a Xi function definition. *)

(** A [toplevel] is a toplevel statement. *)
type toplevel =
  | Use of id
  | Global of Var.stmt
  | Function of fn

type program = toplevel list
(** A [program] is a sequence of toplevel statements. *)

type interface = signature list
(** An [interface] is a Xi interface *)

(** An expression of type [t] is an expression representing a node of
    the Abstract Syntax Tree of a Xi program. *)
type t =
  | Program of program
  | Interface of interface

val array_of_string : string -> expr array
(** [array_of_string s] is the array of Xi expressions representing the
    string literal [s]. *)

val sexp_of_t : t -> Sexp.t
(** [sexp_of_t ast] is the s-expression serialization of [ast]. *)
