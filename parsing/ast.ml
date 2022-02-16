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

type literal =
  | Int of string
  | Bool of bool
  | Char of Uchar.t
  | String of string

type expr =
  | Literal of literal
  | Id of id
  | Array of expr array
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
  | ArrayElt of assign_target * expr

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
  | Return of expr list
  | Block of block

and block = stmt list

type signature = {
  id : id;
  params : decl list;
  types : typ list;
}

type fn = signature * block

type definition =
  | FnDefn of fn
  | GlobalDecl of decl
  | GlobalInit of decl * literal

type source = {
  uses : id list;
  definitions : definition list;
}

type interface = signature list

type t =
  | Source of source
  | Interface of interface

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

(** [sexp_of_gets lhs rhs] is the s-expression serialization of the
    statement [lhs = rhs] *)
let sexp_of_gets lhs rhs = Sexp.List [ Sexp.Atom "="; lhs; rhs ]

(** [sexp_of_use id] is the s-expression serialization of the statement
    [use id] *)
let sexp_of_use id = Sexp.List [ Sexp.Atom "use"; Sexp.Atom id ]

(** [sexp_of_char c] is the s-expression serialization of character [c],
    surrounded by single quotes. *)
let sexp_of_char c =
  Sexp.Atom (c |> Unicode.string_of_uchar |> Printf.sprintf "'%s'")

(** [sexp_of_string c] is the s-expression serialization of string [s],
    surrounded by double quotes. *)
let sexp_of_string s =
  Sexp.Atom (s |> Unicode.escape_string |> Printf.sprintf "\"%s\"")

(** [sexp_of_literal v] is the s-expression serialization of literal
    value [v] *)
let sexp_of_literal = function
  | Char c -> sexp_of_char c
  | String s -> sexp_of_string s
  | Int i -> Sexp.Atom i
  | Bool b -> Bool.sexp_of_t b

let rec sexp_of_t = function
  | Source s -> sexp_of_source s
  | Interface sigs -> sexp_of_interface sigs

(** [sexp_of_source source] is the s-expression serialization of the AST
    [source] *)
and sexp_of_source { uses; definitions } =
  Sexp.List
    [
      List.sexp_of_t sexp_of_use uses;
      List.sexp_of_t sexp_of_definition definitions;
    ]

(** [sexp_of_interface interface] is the s-expression serialization of
    the AST [interface]. *)
and sexp_of_interface sigs =
  Sexp.List [ List.sexp_of_t sexp_of_fn sigs ]

(** [sexp_of_definition def] is the s-expression serialization of global
    definition [def] *)
and sexp_of_definition = function
  | FnDefn (signature, body) -> sexp_of_fn signature ~body
  | GlobalDecl decl -> sexp_of_global decl
  | GlobalInit (decl, init) -> sexp_of_global decl ~init

(** [sexp_of_fn ?body signature] is the s-expression serialization of
    function signature [signature] with an optional function body. *)
and sexp_of_fn ?body { id; params; types } =
  let id = Sexp.Atom id in
  let params = List.sexp_of_t sexp_of_decl params in
  let types = List.sexp_of_t sexp_of_type types in
  let body = Option.map body ~f:(List.sexp_of_t sexp_of_stmt) in
  Sexp.List (id :: params :: types :: Option.to_list body)

(** [sexp_of_stmt stmt] is the s-expression serialization of [stmt] *)
and sexp_of_stmt = function
  | If (e1, s1, s2) -> sexp_of_if e1 s1 s2
  | While (e, s) -> sexp_of_while e s
  | Decl decl -> sexp_of_decl decl
  | Init (decl, e) -> sexp_of_init decl e
  | Assign (target, e) -> sexp_of_assign target e
  | MultiInit (targets, call) -> sexp_of_multi_init targets call
  | ProcCall (id, args) -> sexp_of_call id args
  | Return es -> sexp_of_return es
  | Block stmts -> List.sexp_of_t sexp_of_stmt stmts

(** [sexp_of_decl (id, typ)] is the s-expression serialization of the Xi
    declaration [id: typ] *)
and sexp_of_decl (id, typ) =
  Sexp.List [ Sexp.Atom id; sexp_of_type typ ]

(** [sexp_of_global ?init (id, typ)] is the s-expression serialization
    of the the global Xi declaration with name [id], type [typ] and
    optional initialization expression [init] *)
and sexp_of_global ?init (id, typ) =
  let global = Sexp.Atom ":global" in
  let id_sexp = Sexp.Atom id in
  let type_sexp = sexp_of_type typ in
  let lst = Option.to_list (Option.map init ~f:sexp_of_literal) in
  Sexp.List (global :: id_sexp :: type_sexp :: lst)

(** [sexp_of_if e s1 s2] is the s-expression serialization of if
    statement [if e s1 else s2] where the else clause may or may not be
    present. *)
and sexp_of_if e s1 s2 =
  let lst = Option.to_list (Option.map s2 ~f:sexp_of_stmt) in
  Sexp.List (Sexp.Atom "if" :: sexp_of_expr e :: sexp_of_stmt s1 :: lst)

(** [sexp_of_expr e] is the s-expression serialization of expression [e] *)
and sexp_of_expr = function
  | Id id -> Sexp.Atom id
  | Literal v -> sexp_of_literal v
  | Array arr -> sexp_of_array arr
  | Bop (bop, e1, e2) -> sexp_of_infix_bop bop e1 e2
  | Uop (uop, e) -> sexp_of_uop uop e
  | FnCall (id, args) -> sexp_of_call id args
  | Index (e1, e2) -> sexp_of_index e1 e2

and sexp_of_type = function
  | Type.Primitive Int -> Sexp.Atom "int"
  | Type.Primitive Bool -> Sexp.Atom "bool"
  | Type.Array { contents; length } ->
      let lst = Option.to_list (Option.map length ~f:sexp_of_expr) in
      Sexp.List (Sexp.Atom "[]" :: sexp_of_type contents :: lst)

(** [sexp_of_array arr] is the s-expression serialization of the Xi
    array [arr] *)
and sexp_of_array arr = Array.sexp_of_t sexp_of_expr arr

(** [sexp_of_infix_binop bop e1 e2] is the s-expression serialization of
    the infix binary operation represented by operation [bop] and
    expressions [e1] and [e2]. *)
and sexp_of_infix_bop bop e1 e2 =
  sexp_of_bop (string_of_binop bop) e1 e2

(** [sexp_of_bop s e1 e2] is the s-expression serialization of the
    binary operation represented by operation [s] and expressions [e1]
    and [e2]. *)
and sexp_of_bop s e1 e2 =
  Sexp.List [ Sexp.Atom s; sexp_of_expr e1; sexp_of_expr e2 ]

(** [sexp_of_unop uop e] is the s-expression serialization of the unary
    operation with operator [uop] and expression [e]. *)
and sexp_of_uop uop e =
  let uop_sexp = Sexp.Atom (string_of_unop uop) in
  Sexp.List [ uop_sexp; sexp_of_expr e ]

(** [sexp_of_call id \[e1; ...; en\]] is the s-expression serialization
    of the application of function [id] to [e1, ..., en], i.e. the call
    [id(e1, ..., en)]. *)
and sexp_of_call id args =
  Sexp.List
    (args |> List.map ~f:sexp_of_expr |> List.cons (Sexp.Atom id))

(** [sexp_of_index e1 e2] is the s-expression serialization of the
    indexing of array [e1] at index [e2]. *)
and sexp_of_index e1 e2 = sexp_of_bop "[]" e1 e2

(** [sexp_of_while e s] is the s-expression serialzation of the
    statement [while e s] *)
and sexp_of_while e s =
  Sexp.List [ Sexp.Atom "while"; sexp_of_expr e; sexp_of_stmt s ]

(** [sexp_of_init (id, typ) e] is the s-expression serialization of the
    initialization statement [id: typ = e] *)
and sexp_of_init decl e =
  sexp_of_gets (sexp_of_decl decl) (sexp_of_expr e)

(** [sexp_of_assign target e] is the s-expression serialization of the
    statement [target = e] *)
and sexp_of_assign target e =
  sexp_of_gets (sexp_of_target target) (sexp_of_expr e)

(** [sexp_of_target target] is the s-expression serialization of
    assignment target [target], either an identifier or an array
    element. *)
and sexp_of_target = function
  | Var id -> Sexp.Atom id
  | ArrayElt (target, e) ->
      Sexp.List
        [ Sexp.Atom "[]"; sexp_of_target target; sexp_of_expr e ]

(** [sexp_of_multi_init \[e1; ...; en\] (id, args)] is the s-expression
    serialization of the multiple initialization statement
    [e1, ..., en = id(args)]. *)
and sexp_of_multi_init targets (id, args) =
  sexp_of_gets
    (List.sexp_of_t sexp_of_multi_target targets)
    (sexp_of_call id args)

(** [sexp_of_multi_target target] is the s-expression serialization of
    multiple initialization target [target], either a wildcard or a
    variable. *)
and sexp_of_multi_target = function
  | MultiDecl d -> sexp_of_decl d
  | Wildcard -> Sexp.Atom "_"

(** [sexp_of_return \[e1; ...; en\]] is the s-expression serialization
    of the statement [return e1, ..., en]. *)
and sexp_of_return es =
  Sexp.List (Sexp.Atom "return" :: List.map ~f:sexp_of_expr es)
