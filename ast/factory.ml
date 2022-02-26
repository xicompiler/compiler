open Core
open Abstract

module Make (Ex : Node.S) (St : Node.S) = struct
  type id = string

  module Expr = struct
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

    type primitive =
      | Int of string
      | Bool of bool
      | Char of Uchar.t

    module Node = Ex

    type t =
      | Primitive of primitive
      | Id of id
      | Array of node array
      | String of string
      | Bop of binop * node * node
      | Uop of unop * node
      | FnCall of call
      | Index of node * node

    and node = t Node.t
    and call = id * node list
  end

  type expr = Expr.t

  module Tau = struct
    module N = struct
      type 'a t = 'a * Expr.node option

      let get = fst
    end

    include Tau.Make (N)

    let array contents length = `Array (contents, length)
  end

  module Stmt = struct
    type decl = id * Tau.t

    type assign_target =
      | Var of id
      | ArrayElt of assign_target * Expr.node

    type init_target =
      | InitDecl of decl
      | Wildcard

    type init = init_target * Expr.node

    module Node = St

    type t =
      | If of Expr.node * node * node option
      | While of Expr.node * node
      | Decl of decl
      | Init of init
      | Assign of assign_target * Expr.node
      | MultiInit of init_target list * Expr.call
      | PrCall of Expr.call
      | Return of Expr.node list
      | Block of block

    and node = t Node.t
    and block = node list
  end

  type stmt = Stmt.t

  type signature = {
    id : id;
    params : Stmt.decl list;
    types : Tau.t list;
  }

  type fn = signature * Stmt.block

  type definition =
    | FnDefn of fn
    | GlobalDecl of Stmt.decl
    | GlobalInit of Stmt.decl * Expr.primitive

  type source = {
    uses : id list;
    definitions : definition list;
  }

  type interface = signature list

  type t =
    | Source of source
    | Interface of interface

  open Expr
  open Stmt

  (** [string_of_unop unop] is the string representation of [unop]. *)
  let string_of_unop = function
    | IntNeg -> "-"
    | LogicalNeg -> "!"

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

  (** [sexp_of_gets lhs rhs] is the s-expression serialization of the
      statement [lhs = rhs] *)
  let sexp_of_gets lhs rhs = Sexp.List [ Sexp.Atom "="; lhs; rhs ]

  (** [sexp_of_use id] is the s-expression serialization of the
      statement [use id] *)
  let sexp_of_use id = Sexp.List [ Sexp.Atom "use"; Sexp.Atom id ]

  (** [sexp_of_char c] is the s-expression serialization of character
      [c], surrounded by single quotes. *)
  let sexp_of_char c =
    Sexp.Atom (c |> Unicode.string_of_uchar |> Printf.sprintf "'%s'")

  (** [sexp_of_string c] is the s-expression serialization of string
      [s], surrounded by double quotes. *)
  let sexp_of_string s =
    Sexp.Atom (s |> Unicode.escape_string |> Printf.sprintf "\"%s\"")

  (** [sexp_of_primitive v] is the s-expression serialization of literal
      value [v] *)
  let sexp_of_primitive = function
    | Char c -> sexp_of_char c
    | Int i -> Sexp.Atom i
    | Bool b -> Bool.sexp_of_t b

  (** [sexp_of_expr e] is the s-expression serialization of expression
      [e] *)
  let rec sexp_of_expr = function
    | Id id -> Sexp.Atom id
    | Primitive v -> sexp_of_primitive v
    | Array arr -> sexp_of_array arr
    | String s -> sexp_of_string s
    | Bop (bop, e1, e2) -> sexp_of_infix_bop bop e1 e2
    | Uop (uop, e) -> sexp_of_uop uop e
    | FnCall (id, args) -> sexp_of_call id args
    | Index (e1, e2) -> sexp_of_index e1 e2

  (** [sexp_of_enode node] is the s-expression serialization of the
      expression wrapped in node [node] *)
  and sexp_of_enode node = node |> Expr.Node.get |> sexp_of_expr

  (** [sexp_of_array arr] is the s-expression serialization of the Xi
      array [arr] *)
  and sexp_of_array arr = Array.sexp_of_t sexp_of_enode arr

  (** [sexp_of_infix_binop bop e1 e2] is the s-expression serialization
      of the infix binary operation represented by operation [bop] and
      expressions [e1] and [e2]. *)
  and sexp_of_infix_bop bop e1 e2 =
    sexp_of_bop (string_of_binop bop) e1 e2

  (** [sexp_of_bop s e1 e2] is the s-expression serialization of the
      binary operation represented by operation [s] and expressions [e1]
      and [e2]. *)
  and sexp_of_bop s e1 e2 =
    Sexp.List [ Sexp.Atom s; sexp_of_enode e1; sexp_of_enode e2 ]

  (** [sexp_of_unop uop e] is the s-expression serialization of the
      unary operation with operator [uop] and expression [e]. *)
  and sexp_of_uop uop e =
    let uop_sexp = Sexp.Atom (string_of_unop uop) in
    Sexp.List [ uop_sexp; sexp_of_enode e ]

  (** [sexp_of_call id \[e1; ...; en\]] is the s-expression
      serialization of the application of function [id] to
      [e1, ..., en], i.e. the call [id(e1, ..., en)]. *)
  and sexp_of_call id args =
    Sexp.List
      (args |> List.map ~f:sexp_of_enode |> List.cons (Sexp.Atom id))

  (** [sexp_of_index e1 e2] is the s-expression serialization of the
      indexing of array [e1] at index [e2]. *)
  and sexp_of_index e1 e2 = sexp_of_bop "[]" e1 e2

  (** [sexp_of_type t] is the s-expression serialization of type [t] *)
  let rec sexp_of_type = function
    | `Int -> Sexp.Atom "int"
    | `Bool -> Sexp.Atom "bool"
    | `Array (contents, length) ->
        let lst = Option.to_list (Option.map length ~f:sexp_of_enode) in
        Sexp.List (Sexp.Atom "[]" :: sexp_of_type contents :: lst)

  (** [sexp_of_decl (id, typ)] is the s-expression serialization of the
      Xi declaration [id: typ] *)
  let sexp_of_decl (id, typ) =
    Sexp.List [ Sexp.Atom id; sexp_of_type typ ]

  (** [sexp_of_target target] is the s-expression serialization of
      assignment target [target], either an identifier or an array
      element. *)
  let rec sexp_of_target = function
    | Var id -> Sexp.Atom id
    | ArrayElt (target, e) ->
        Sexp.List
          [ Sexp.Atom "[]"; sexp_of_target target; sexp_of_enode e ]

  (** [sexp_of_assign target e] is the s-expression serialization of the
      statement [target = e] *)
  let sexp_of_assign target e =
    sexp_of_gets (sexp_of_target target) (sexp_of_enode e)

  (** [sexp_of_init_target] is the s-expression serialization of
      initialization target [target], either a wildcard or a variable. *)
  let sexp_of_init_target = function
    | InitDecl d -> sexp_of_decl d
    | Wildcard -> Sexp.Atom "_"

  (** [sexp_of_init (id, typ) e] is the s-expression serialization of
      the initialization statement [id: typ = e] *)
  let sexp_of_init target e =
    sexp_of_gets (sexp_of_init_target target) (sexp_of_enode e)

  (** [sexp_of_multi_init \[e1; ...; en\] (id, args)] is the
      s-expression serialization of the multiple initialization
      statement [e1, ..., en = id(args)]. *)
  let sexp_of_multi_init targets (id, args) =
    sexp_of_gets
      (List.sexp_of_t sexp_of_init_target targets)
      (sexp_of_call id args)

  (** [sexp_of_stmt stmt] is the s-expression serialization of [stmt] *)
  let rec sexp_of_stmt = function
    | If (e1, s1, s2) -> sexp_of_if e1 s1 s2
    | While (e, s) -> sexp_of_while e s
    | Decl decl -> sexp_of_decl decl
    | Init (decl, e) -> sexp_of_init decl e
    | Assign (target, e) -> sexp_of_assign target e
    | MultiInit (targets, call) -> sexp_of_multi_init targets call
    | PrCall (id, args) -> sexp_of_call id args
    | Return es -> sexp_of_return es
    | Block stmts -> sexp_of_stmts stmts

  (** [sexp_of_stmts stmts] is the s-expression serialization of
      sequenced statements [stmts] *)
  and sexp_of_stmts stmts = List.sexp_of_t sexp_of_snode stmts

  (** [sexp_of_snode node] is the s-expression serialization of the stmt
      wrapped in [mode]*)
  and sexp_of_snode node = node |> Stmt.Node.get |> sexp_of_stmt

  (** [sexp_of_if e s1 s2] is the s-expression serialization of if
      statement [if e s1 else s2] where the else clause may or may not
      be present. *)
  and sexp_of_if e s1 s2 =
    let lst = Option.to_list (Option.map s2 ~f:sexp_of_snode) in
    Sexp.List
      (Sexp.Atom "if" :: sexp_of_enode e :: sexp_of_snode s1 :: lst)

  (** [sexp_of_while e s] is the s-expression serialzation of the
      statement [while e s] *)
  and sexp_of_while e s =
    Sexp.List [ Sexp.Atom "while"; sexp_of_enode e; sexp_of_snode s ]

  (** [sexp_of_return \[e1; ...; en\]] is the s-expression serialization
      of the statement [return e1, ..., en]. *)
  and sexp_of_return es =
    Sexp.List (Sexp.Atom "return" :: List.map ~f:sexp_of_enode es)

  (** [sexp_of_expr_stmt e] is the s-expression serialization of the
      statement [_ = e]. *)
  and sexp_of_expr_stmt e =
    sexp_of_gets (Sexp.Atom "_") (sexp_of_enode e)

  (** [sexp_of_fn ?body signature] is the s-expression serialization of
      function signature [signature] with an optional function body. *)
  let sexp_of_fn ?body { id; params; types } =
    let id = Sexp.Atom id in
    let params = List.sexp_of_t sexp_of_decl params in
    let types = List.sexp_of_t sexp_of_type types in
    let body = Option.map body ~f:sexp_of_stmts in
    Sexp.List (id :: params :: types :: Option.to_list body)

  (** [sexp_of_global ?init (id, typ)] is the s-expression serialization
      of the the global Xi declaration with name [id], type [typ] and
      optional initialization expression [init] *)
  let sexp_of_global ?init (id, typ) =
    let global = Sexp.Atom ":global" in
    let id_sexp = Sexp.Atom id in
    let type_sexp = sexp_of_type typ in
    let lst = Option.to_list (Option.map init ~f:sexp_of_primitive) in
    Sexp.List (global :: id_sexp :: type_sexp :: lst)

  (** [sexp_of_definition def] is the s-expression serialization of
      global definition [def] *)
  let sexp_of_definition = function
    | FnDefn (signature, body) -> sexp_of_fn signature ~body
    | GlobalDecl decl -> sexp_of_global decl
    | GlobalInit (decl, init) -> sexp_of_global decl ~init

  (** [sexp_of_source source] is the s-expression serialization of the
      AST [source] *)
  let sexp_of_source { uses; definitions } =
    Sexp.List
      [
        List.sexp_of_t sexp_of_use uses;
        List.sexp_of_t sexp_of_definition definitions;
      ]

  (** [sexp_of_interface interface] is the s-expression serialization of
      the AST [interface]. *)
  let sexp_of_interface sigs =
    Sexp.List [ List.sexp_of_t sexp_of_fn sigs ]

  let sexp_of_t = function
    | Source s -> sexp_of_source s
    | Interface sigs -> sexp_of_interface sigs
end
