open Base

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
  | Id of string
  | Int of int64
  | Bool of bool
  | Array of expr array
  | Bop of binop * expr * expr
  | Uop of unop * expr
  | App of id * expr list
  | Index of expr * expr

module Var = struct
  type decl = id * Type.t

  type assignee =
    | Existing of id
    | New of decl
    | Wildcard

  type assign = assignee list * expr list

  type stmt =
    | Assign of assign
    | Decl of decl list
end

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

type fn = signature * stmt list

type toplevel =
  | Use of id
  | Global of Var.stmt
  | Function of fn

type program = toplevel list

type interface = signature list

type t =
  | Program of program
  | Interface of interface

let array_of_string s =
  let folder lst u =
    let i = u |> Caml.Uchar.to_int |> Int64.of_int in
    Int i :: lst
  in
  s |> Unicode.fold folder [] |> List.rev |> Array.of_list

(** [string_of_binop bop] is the string representation of [bop]. *)
let string_of_binop = function
  | Mult -> "*"
  | HighMult -> "*>>"
  | Div -> "/"
  | Mod -> "%"
  | Plus -> "+"
  | Minus -> "-"
  | Lt -> "<"
  | Leq -> "<="
  | Geq -> ">="
  | Gt -> ">"
  | Eq -> "=="
  | Neq -> "!="
  | And -> "&"
  | Or -> "|"

(** [string_of_unop unop] is the string representation of [unop]. *)
let string_of_unop = function
  | IntNeg -> "-"
  | LogicalNeg -> "!"

let rec sexp_of_t = function
  | Program p -> sexp_of_program p
  | Interface i -> sexp_of_interface i

(** [sexp_of_interface interface] is the s-expression serialization of
    [interface]. *)
and sexp_of_interface interface =
  List.sexp_of_t sexp_of_signature interface

(** [sexp_of_signature signature] is the s-expression serialization of
    [signature]. *)
and sexp_of_signature signature =
  Sexp.List
    [
      Sexp.Atom signature.id;
      List.sexp_of_t sexp_of_decl signature.args;
      List.sexp_of_t sexp_of_type signature.types;
    ]

(** [sexp_of_program stmts] is the s-expression serialization of
    [stmts]. *)
and sexp_of_program stmts = List.sexp_of_t sexp_of_toplevel stmts

(** [sexp_of_toplevel toplevel] is the s-expression serialization of
    [toplevel]. *)
and sexp_of_toplevel = function
  | Use id -> Sexp.List [ Sexp.Atom "use"; Sexp.Atom id ]
  | Global stmt -> sexp_of_varstmt stmt
  | Function fn -> sexp_of_fn fn

(** [sexp_of_varstmt varstmt] is the s-expression serialization of
    [varstmt]. *)
and sexp_of_varstmt = function
  | Assign assn -> sexp_of_assign assn
  | Decl dlist -> List.sexp_of_t sexp_of_decl dlist

(** [sexp_of_fn (signature, stmts)] is the s-expression serialization of
    the function [(signature, stmts)]. *)
and sexp_of_fn (signature, stmts) =
  Sexp.List
    [
      Sexp.Atom signature.id;
      List.sexp_of_t sexp_of_decl signature.args;
      List.sexp_of_t sexp_of_type signature.types;
      List.sexp_of_t sexp_of_stmt stmts;
    ]

(** [sexp_of_stmt] is the s-expression serialization of [stmt]. *)
and sexp_of_stmt = function
  | Block stmts -> List.sexp_of_t sexp_of_stmt stmts
  | Var stmt -> sexp_of_varstmt stmt
  | If (expr, stmt1, stmt2) -> sexp_of_if expr stmt1 stmt2
  | While (expr, stmt) -> sexp_of_while expr stmt
  | Return exprs -> sexp_of_return exprs
  | Proc (id, exprs) -> sexp_of_proc id exprs

(** [sexp_of_while expr] is the s-expression serialization of the while
    lo[expr] and [stmt]. *)
and sexp_of_while expr stmt =
  Sexp.List [ Sexp.Atom "while"; sexp_of_expr expr; sexp_of_stmt stmt ]

(** [sexp_of_return exprs] is the s-expression serialization of the
    statement that returns [exprs]. *)
and sexp_of_return exprs =
  Sexp.List [ Sexp.Atom "return"; List.sexp_of_t sexp_of_expr exprs ]

(** [sexp_of_return proc] is the s-expression serialization of the
    procedure represented by [id] and [exprs]. *)
and sexp_of_proc id exprs =
  Sexp.List [ Sexp.Atom id; List.sexp_of_t sexp_of_expr exprs ]

(** [sexp_of_if expr stmt1 stmt2] is the s-expression serialization of
    the if statement represented by [expr], [stmt1], and [stmt2]. *)
and sexp_of_if expr stmt1 stmt2 =
  let lst = Option.to_list (Option.map stmt2 ~f:sexp_of_stmt) in
  Sexp.List (Sexp.Atom "if" :: sexp_of_stmt stmt1 :: lst)

(** [sexp_of_type typ] is the s-expression serialization of [typ]. *)
and sexp_of_type typ = Sexp.Atom (Type.to_string typ)

(** [sexp_of_assign (assignees, exprs)] is the s-expression
    serialization of the assignment of [assignees] to [exprs]. *)
and sexp_of_assign (assignees, exprs) =
  Sexp.List
    [
      Sexp.Atom "=";
      List.sexp_of_t sexp_of_assignee assignees;
      List.sexp_of_t sexp_of_expr exprs;
    ]

(** [sexp_of_assignee assignee] is the s-expression serialization of the
    [assignee]. *)
and sexp_of_assignee = function
  | Existing id -> Sexp.Atom id
  | New decl -> sexp_of_decl decl
  | Wildcard -> Sexp.Atom "_"

(** [sexp_of_decl (id, typ)] is the s-expression serialization of the
    declaration of [id] with [typ]. *)
and sexp_of_decl (id, typ) =
  Sexp.List [ Sexp.Atom id; Sexp.Atom (Type.to_string typ) ]

(** [sexp_of_expr expr] is the s-expression serialization of [expr] *)
and sexp_of_expr = function
  | Id id -> Sexp.Atom id
  | Int i -> Int64.sexp_of_t i
  | Bool b -> Bool.sexp_of_t b
  | Array arr -> sexp_of_array arr
  | Bop (bop, e1, e2) -> sexp_of_infix_binop bop e1 e2
  | Uop (uop, e) -> sexp_of_unop uop e
  | App (id, args) -> sexp_of_app id args
  | Index (e1, e2) -> sexp_of_index e1 e2

(** [sexp_of_array arr] is the s-expression serialization of [arr] *)
and sexp_of_array arr = Array.sexp_of_t sexp_of_expr arr

(** [sexp_of_binop s e1 e2] is the s-expression serialization of the
    binary operation represented by operation [s] and expressions [e1]
    and [e2]. *)
and sexp_of_binop s e1 e2 =
  Sexp.List [ Sexp.Atom s; sexp_of_expr e1; sexp_of_expr e2 ]

(** [sexp_of_infix_binop bop e1 e2] is the s-expression serialization of
    the infix binary operation represented by operation [bop] and
    expressions [e1] and [e2]. *)
and sexp_of_infix_binop bop e1 e2 =
  sexp_of_binop (string_of_binop bop) e1 e2

(** [sexp_of_index e1 e2] is the s-expression serialization of the
    indexing of array [e1] at index [e2]. *)
and sexp_of_index e1 e2 = sexp_of_binop "[]" e1 e2

(** [sexp_of_unop uop e] is the s-expression serialization of the unary
    operation with operator [uop] and expression [e]. *)
and sexp_of_unop uop e =
  let uop_sexp = Sexp.Atom (string_of_unop uop) in
  Sexp.List [ uop_sexp; sexp_of_expr e ]

(** [sexp_off_app id args] is the s-expression serialization of the
    application of function [id] to [args]. *)
and sexp_of_app id args =
  Sexp.List
    (args |> List.map ~f:sexp_of_expr |> List.cons (Sexp.Atom id))
