open Core
open Abstract

module Make (Ex : Node.S) (St : Node.S) = struct
  type id = string

  module Expr = struct
    include Op
    include Primitive
    module Node = Ex

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
    and nodes = node list
    and call = id * nodes
    and index = node * node
  end

  type expr = Expr.t

  module Stmt = struct
    type decl = id * Type.tau

    module Node = St

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
    and block = node list
  end

  type stmt = Stmt.t
  type signature = { id : id; params : Stmt.decl list; types : Type.tau list }
  type fn = signature * Stmt.block

  type definition =
    | FnDefn of fn
    | GlobalDecl of Stmt.decl
    | GlobalInit of id * Type.tau * Expr.primitive

  type source = { uses : id list; definitions : definition list }
  type interface = signature list
  type t = Source of source | Interface of interface

  open Expr
  open Stmt

  let int_bound = "9223372036854775808"

  (** [string_of_unop unop] is the string representation of [unop]. *)
  let string_of_unop = function IntNeg -> "-" | LogicalNeg -> "!"

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

  (** [sexp_of_id id] is [Sexp.Atom id]*)
  let sexp_of_id id = Sexp.Atom id

  (** [sexp_of_gets lhs rhs] is the s-expression serialization of the
      statement [lhs = rhs] *)
  let sexp_of_gets lhs rhs = Sexp.List [ Sexp.Atom "="; lhs; rhs ]

  (** [sexp_of_use id] is the s-expression serialization of the
      statement [use id] *)
  let sexp_of_use id = Sexp.List [ Sexp.Atom "use"; sexp_of_id id ]

  (** [sexp_of_char c] is the s-expression serialization of character
      [c], surrounded by single quotes. *)
  let sexp_of_char c =
    Sexp.Atom (c |> Unicode.string_of_uchar |> Printf.sprintf "'%s'")

  (** [sexp_of_string c] is the s-expression serialization of string
      [s], surrounded by double quotes. *)
  let sexp_of_string s =
    Sexp.Atom (s |> Unicode.escape_string |> Printf.sprintf "\"%s\"")

  (** [sexp_of_expr e] is the s-expression serialization of expression
      [e] *)
  let rec sexp_of_expr = function
    | Id id -> sexp_of_id id
    | Primitive v -> sexp_of_primitive v
    | Array arr -> sexp_of_array arr
    | String s -> sexp_of_string s
    | Bop (bop, e1, e2) -> sexp_of_infix_bop bop e1 e2
    | Uop (uop, e) -> sexp_of_uop uop e
    | FnCall (id, args) -> sexp_of_call id args
    | Length e -> sexp_of_length e
    | Index (e1, e2) -> sexp_of_index e1 e2

  (** [sexp_of_primitive v] is the s-expression serialization of literal
      value [v] *)
  and sexp_of_primitive = function
    | IntBound -> Sexp.Atom int_bound
    | Char c -> sexp_of_char c
    | Int i ->
        if Int64.is_negative i then
          let s =
            if Int64.equal Int64.min_value i then int_bound
            else Int64.to_string (Int64.neg i)
          in
          Sexp.List [ Sexp.Atom "-"; Sexp.Atom s ]
        else Sexp.Atom (Int64.to_string i)
    | Bool b -> Bool.sexp_of_t b

  (** [sexp_of_enode node] is the s-expression serialization of the
      expression wrapped in node [node] *)
  and sexp_of_enode node = node |> Expr.Node.get |> sexp_of_expr

  (** [sexp_of_array arr] is the s-expression serialization of the Xi
      array [arr] *)
  and sexp_of_array arr = Array.sexp_of_t sexp_of_enode arr

  (** [sexp_of_infix_binop bop e1 e2] is the s-expression serialization
      of the infix binary operation represented by operation [bop] and
      expressions [e1] and [e2]. *)
  and sexp_of_infix_bop bop e1 e2 = sexp_of_bop (string_of_binop bop) e1 e2

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
    Sexp.List (args |> List.map ~f:sexp_of_enode |> List.cons (sexp_of_id id))

  (** [sexo_of_length e] is the s-expression serialization of the
      expression [length(e)] *)
  and sexp_of_length e = sexp_of_call "length" [ e ]

  (** [sexp_of_index e1 e2] is the s-expression serialization of the
      indexing of array [e1] at index [e2]. *)
  and sexp_of_index e1 e2 = sexp_of_bop "[]" e1 e2

  (** [sexp_of_prim_type t] is the s-expression serialization of
      primitive type [t] *)
  let sexp_of_prim_type = function
    | `Int -> Sexp.Atom "int"
    | `Bool -> Sexp.Atom "bool"

  (** [sexp_of_type t] is the s-expression serialization of type [t] *)
  let rec sexp_of_type = function
    | `Array t -> Sexp.List [ Sexp.Atom "[]"; sexp_of_type t ]
    | (`Int | `Bool) as prim -> sexp_of_prim_type prim

  (** [hd_tl_exn lst] is [h :: t] if [lst] is [h :: t]. Raises:
      [Failure] if [lst] is nil. *)
  let hd_tl_exn = function h :: t -> (h, t) | [] -> failwith "list empty"

  (** [sexp_of_decl_type typ lengths] is the s-expression serialization
      of the array type [t\[l1\]\[l2\]...] where each length is optional *)
  let rec sexp_of_decl_type typ lengths =
    match typ with
    | `Array t ->
        let e, es = hd_tl_exn lengths in
        let lst = Option.to_list (Option.map e ~f:sexp_of_enode) in
        Sexp.List (Sexp.Atom "[]" :: sexp_of_decl_type t es :: lst)
    | (`Int | `Bool) as prim -> sexp_of_prim_type prim

  (** [sexp_of_decl id typ] is the s-expression serialization of the
      statement [id: typ]. *)
  let sexp_of_decl id typ = Sexp.List [ sexp_of_id id; typ ]

  (** [sexp_of_array_decl id `Array (`Array ... (prim)) \[l1; ... lm\]]
      is the s-expression serialization of the statement
      [id: prim\[l1\]\[l2\]...]*)
  let sexp_of_array_decl id typ lengths =
    sexp_of_decl id (sexp_of_decl_type typ lengths)

  (** [sexp_of_var_decl (id, typ)] is the s-expression serialization of
      the Xi declaration [id: typ] *)
  let sexp_of_var_decl id typ = sexp_of_decl id (sexp_of_type typ)

  (** [sexp_of_assign lhs e] is the s-expression serialization of the
      assignment statement [lhs = e]. *)
  let sexp_of_assign lhs e = sexp_of_gets lhs (sexp_of_enode e)

  (** [sexp_of_var_assign id e] is the s-expression serialization of the
      statement [id = e] *)
  let sexp_of_var_assign id = sexp_of_assign (sexp_of_id id)

  (** [sexp_of_arr_assign e1 e2 e3] is the s-expression serialization of
      the statement [e1\[e2\] = e3] *)
  let sexp_of_arr_assign e1 e2 = sexp_of_assign (sexp_of_index e1 e2)

  (** [wildcard] is the s-expression representing a wildcard character. *)
  let wildcard = Sexp.Atom "_"

  (** [sexp_of_d d] is the s-expression serialization of initialization
      target [d], either a wildcard or a variable. *)
  let sexp_of_d = function
    | Some (id, typ) -> sexp_of_var_decl id typ
    | None -> wildcard

  (** [sexp_of_init id typ e] is the s-expression serialization of the
      initialization statement [id: typ = e] *)
  let sexp_of_init id typ e =
    sexp_of_gets (sexp_of_var_decl id typ) (sexp_of_enode e)

  (** [sexp_of_multi_assign \[e1; ...; en\] id args] is the s-expression
      serialization of the multiple initialization statement
      [e1, ..., en = id(args)]. *)
  let sexp_of_multi_assign ds id args =
    sexp_of_gets (List.sexp_of_t sexp_of_d ds) (sexp_of_call id args)

  (** [sexp_of_stmt stmt] is the s-expression serialization of [stmt] *)
  let rec sexp_of_stmt = function
    | If (e, s) -> sexp_of_if e s
    | IfElse (e, s1, s2) -> sexp_of_if_else e s1 s2
    | While (e, s) -> sexp_of_while e s
    | VarDecl (id, typ) -> sexp_of_var_decl id typ
    | ArrayDecl (id, typ, lengths) -> sexp_of_array_decl id typ lengths
    | ExprStmt (id, args) -> sexp_of_expr_stmt id args
    | VarInit (id, typ, e) -> sexp_of_init id typ e
    | Assign (id, e) -> sexp_of_var_assign id e
    | ArrAssign (e1, e2, e3) -> sexp_of_arr_assign e1 e2 e3
    | MultiAssign (ds, id, args) -> sexp_of_multi_assign ds id args
    | PrCall (id, args) -> sexp_of_call id args
    | Return es -> sexp_of_return es
    | Block stmts -> sexp_of_stmts stmts

  (** [sexp_of_stmts stmts] is the s-expression serialization of
      sequenced statements [stmts] *)
  and sexp_of_stmts stmts = List.sexp_of_t sexp_of_snode stmts

  (** [sexp_of_snode node] is the s-expression serialization of the stmt
      wrapped in [mode]*)
  and sexp_of_snode node = node |> Stmt.Node.get |> sexp_of_stmt

  and sexp_of_cond e s lst =
    Sexp.List (Sexp.Atom "if" :: sexp_of_enode e :: sexp_of_snode s :: lst)

  (** [sexp_of_if e s1] is the s-expression serialization of if
      statement [if e s1] *)
  and sexp_of_if e s = sexp_of_cond e s []

  (** [sexp_of_if_else e s1 s2] is the s-expression serialization of the
      statement [if e s1 else s2] *)
  and sexp_of_if_else e s1 s2 = sexp_of_cond e s1 [ sexp_of_snode s2 ]

  (** [sexp_of_while e s] is the s-expression serialzation of the
      statement [while e s] *)
  and sexp_of_while e s =
    Sexp.List [ Sexp.Atom "while"; sexp_of_enode e; sexp_of_snode s ]

  (** [sexp_of_return \[e1; ...; en\]] is the s-expression serialization
      of the statement [return e1, ..., en]. *)
  and sexp_of_return es =
    Sexp.List (Sexp.Atom "return" :: List.map ~f:sexp_of_enode es)

  (** [sexp_of_expr_stmt id \[e1; ...; en\]] is the s-expression
      serialization of the statement [_ = id(e1, ..., en)]. *)
  and sexp_of_expr_stmt id args =
    sexp_of_gets (Sexp.Atom "_") (sexp_of_call id args)

  (** [sexp_of_fn ?body signature] is the s-expression serialization of
      function signature [signature] with an optional function body. *)
  let sexp_of_fn ?body { id; params; types } =
    let id = sexp_of_id id in
    let sexp_of_param (id, typ) = sexp_of_var_decl id typ in
    let params = List.sexp_of_t sexp_of_param params in
    let types = List.sexp_of_t sexp_of_type types in
    let body = Option.map body ~f:sexp_of_stmts in
    Sexp.List (id :: params :: types :: Option.to_list body)

  (** [sexp_of_global ?init id typ] is the s-expression serialization of
      the the global Xi declaration with name [id], type [typ] and
      optional initialization expression [init] *)
  let sexp_of_global ?init id typ =
    let global = Sexp.Atom ":global" in
    let id_sexp = sexp_of_id id in
    let type_sexp = sexp_of_type typ in
    let lst = Option.to_list (Option.map init ~f:sexp_of_primitive) in
    Sexp.List (global :: id_sexp :: type_sexp :: lst)

  (** [sexp_of_definition def] is the s-expression serialization of
      global definition [def] *)
  let sexp_of_definition = function
    | FnDefn (signature, body) -> sexp_of_fn signature ~body
    | GlobalDecl (id, typ) -> sexp_of_global id typ
    | GlobalInit (id, typ, init) -> sexp_of_global id typ ~init

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
  let sexp_of_interface sigs = Sexp.List [ List.sexp_of_t sexp_of_fn sigs ]

  let sexp_of_t = function
    | Source s -> sexp_of_source s
    | Interface sigs -> sexp_of_interface sigs
end
