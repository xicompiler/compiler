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
  | Char of Uchar.t
  | String of string
  | Bop of binop * expr * expr
  | Uop of unop * expr
  | FnCall of call
  | Index of expr * expr

and call = id * expr list

type typ = expr Type.t

type decl = id * typ

type init = decl * expr

type assign_target =
  | Var of id
  | ArrayElt of id * expr

type multi_target =
  | MultiDecl of decl
  | Wildcard

type stmt =
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Decl of decl
  | Init of init
  | Assign of assign_target * expr
  | MultiInit of multi_target list * call
  | ProcCall of call
  | Block of block

and block = {
  body : stmt list;
  return : expr list option;
}

type signature = {
  id : id;
  params : decl list;
  types : typ list;
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
