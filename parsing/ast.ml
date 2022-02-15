open Core

type id = string

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

type expr =
  | Id of id
  | Int of string
  | Bool of bool
  | Array of expr array
  | String of string
  | Bop of binop * expr * expr
  | Uop of unop * expr
  | FnCall of call
  | Index of expr * expr

and call = id * expr list

type decl = id * Type.t

type init = decl * expr

type multi_assignee =
  | Var of decl
  | Wildcard

type stmt =
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Decl of decl
  | Init of init
  | Assign of id * expr
  | MultiInit of multi_assignee list * call
  | ProcCall of call
  | Block of block

and block = {
  body : stmt list;
  return : expr option option;
}

type signature = {
  id : id;
  params : decl list;
  types : Type.t list;
}

type fn = {
  signature : signature;
  body : block;
}

type definition =
  | FnDefn of fn
  | GlobalDecl of decl
  | GlobalInit of init

type source = {
  uses : id list;
  definitions : definition list;
}

type interface = signature list

type t =
  | Source of source
  | Interface of interface
