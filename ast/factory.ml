open Core
open Abstract

let string_of_binop = function
  | `Mult -> "*"
  | `HighMult -> "*>>"
  | `Div -> "/"
  | `Mod -> "%"
  | `Plus -> "+"
  | `Minus -> "-"
  | `Lt -> "<"
  | `Leq -> "<="
  | `Geq -> ">="
  | `Gt -> ">"
  | `Eq -> "=="
  | `Neq -> "!="
  | `And -> "&"
  | `Or -> "|"

module Make (Types : Types.S) (Ex : Node.S) (St : Node.S) = struct
  include Types

  type id = string

  module Expr = struct
    type equality =
      [ `Eq
      | `Neq
      ]

    type logical =
      [ `And
      | `Or
      ]

    type compare =
      [ `Leq
      | `Lt
      | `Gt
      | `Geq
      ]

    type arith =
      [ `Mult
      | `HighMult
      | `Div
      | `Mod
      | `Plus
      | `Minus
      ]

    type binop =
      [ equality
      | logical
      | compare
      | arith
      ]

    type _ literal =
      | Int : string -> integer literal
      | Bool : bool -> boolean literal
      | Char : Uchar.t -> integer literal
      | String : string -> integer vector literal

    module Node = Ex

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
    and call = id * wrap list
    and wrap = Wrap : _ node -> wrap

    let return node = Wrap node
  end

  type 'a expr = 'a Expr.t

  module Type = struct
    type nonrec primitive =
      | Int
      | Bool

    type t =
      | Primitive of primitive
      | Array of t * integer Expr.node option

    let array contents length = Array (contents, length)
  end

  module Stmt = struct
    type decl = id * Type.t
    type 'a init = decl * 'a Expr.node

    type assign_target =
      | Var of id
      | ArrayElt of assign_target * integer Expr.node

    type multi_target =
      | MultiDecl of decl
      | Wildcard

    module Node = St

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
    and block = node list
  end

  type stmt = Stmt.t

  type signature = {
    id : id;
    params : Stmt.decl list;
    types : Type.t list;
  }

  type fn = signature * Stmt.block

  type definition =
    | FnDefn of fn
    | GlobalDecl of Stmt.decl
    | GlobalInit : Stmt.decl * 'a Expr.literal -> definition

  type source = {
    uses : id list;
    definitions : definition list;
  }

  type interface = signature list

  type t =
    | Source of source
    | Interface of interface

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

  open Expr
  open Stmt

  (** [sexp_of_literal v] is the s-expression serialization of literal
      value [v] *)
  let sexp_of_literal : type a. a literal -> Sexp.t = function
    | Char c -> sexp_of_char c
    | String s -> sexp_of_string s
    | Int i -> Sexp.Atom i
    | Bool b -> Bool.sexp_of_t b

  (** [sexp_of_expr e] is the s-expression serialization of expression
      [e] *)
  let rec sexp_of_expr : type a. a expr -> Sexp.t = function
    | Id id -> Sexp.Atom id
    | Literal v -> sexp_of_literal v
    | Array arr -> sexp_of_array arr
    | Equality (bop, e1, e2) -> sexp_of_infix_bop (bop :> binop) e1 e2
    | Logical (bop, e1, e2) -> sexp_of_infix_bop (bop :> binop) e1 e2
    | Compare (bop, e1, e2) -> sexp_of_infix_bop (bop :> binop) e1 e2
    | Arith (bop, e1, e2) -> sexp_of_infix_bop (bop :> binop) e1 e2
    | LogicalNeg e -> sexp_of_logical_neg e
    | IntNeg e -> sexp_of_int_neg e
    | FnCall (id, args) -> sexp_of_call id args
    | Index (e1, e2) -> sexp_of_index e1 e2

  (** [sexp_of_enode node] is the s-expression serialization of AST
      expression node [node] *)
  and sexp_of_enode : type a. a Expr.node -> Sexp.t =
   fun node -> node |> Expr.Node.get |> sexp_of_expr

  (** [sexp_of_wrap wrap] is the s-expression serialization of the
      [expr] wrapped in existential wrapper [wrap] *)
  and sexp_of_wrap (Wrap node) = sexp_of_enode node

  (** [sexp_of_wrap_list es] is the s-expression serialization of the
      list of expressions corresponding to the list of existential
      wrappers [es] *)
  and sexp_list_of_wrap_list es = List.map ~f:sexp_of_wrap es

  (** [sexp_of_array arr] is the s-expression serialization of the Xi
      array [arr] *)
  and sexp_of_array : type a. a Expr.node array -> Sexp.t =
   fun es -> Array.sexp_of_t sexp_of_enode es

  (** [sexp_of_infix_binop bop e1 e2] is the s-expression serialization
      of the infix binary operation represented by operation [bop] and
      expressions [e1] and [e2]. *)
  and sexp_of_infix_bop :
      type a b. binop -> a Expr.node -> b Expr.node -> Sexp.t =
   fun bop e1 e2 -> sexp_of_bop (string_of_binop bop) e1 e2

  (** [sexp_of_bop s e1 e2] is the s-expression serialization of the
      binary operation represented by operation [s] and expressions [e1]
      and [e2]. *)
  and sexp_of_bop :
      type a b. string -> a Expr.node -> b Expr.node -> Sexp.t =
   fun s e1 e2 ->
    Sexp.List [ Sexp.Atom s; sexp_of_enode e1; sexp_of_enode e2 ]

  (** [sexp_of_logical_neg e] is the s-expression serialization of [!e] *)
  and sexp_of_logical_neg e = sexp_of_uop "!" e

  (** [sexp_of_int_neg e] is the s-expression serialization of [-e] *)
  and sexp_of_int_neg e = sexp_of_uop "-" e

  (** [sexp_of_unop uop e] is the s-expression serialization of the
      unary operation with operator [uop] and expression [e]. *)
  and sexp_of_uop : type a. string -> a Expr.node -> Sexp.t =
   fun uop e -> Sexp.List [ Sexp.Atom uop; sexp_of_enode e ]

  (** [sexp_of_call id \[e1; ...; en\]] is the s-expression
      serialization of the application of function [id] to
      [e1, ..., en], i.e. the call [id(e1, ..., en)]. *)
  and sexp_of_call id args =
    Sexp.List
      (args |> sexp_list_of_wrap_list |> List.cons (Sexp.Atom id))

  (** [sexp_of_index e1 e2] is the s-expression serialization of the
      indexing of array [e1] at index [e2]. *)
  and sexp_of_index : type a b. a Expr.node -> b Expr.node -> Sexp.t =
   fun e1 e2 -> sexp_of_bop "[]" e1 e2

  (** [sexp_of_type t] is the s-expression serialization of type [t] *)
  let rec sexp_of_type = function
    | Type.Primitive Int -> Sexp.Atom "int"
    | Type.Primitive Bool -> Sexp.Atom "bool"
    | Type.Array (contents, length) ->
        let lst = Option.to_list (Option.map length ~f:sexp_of_enode) in
        Sexp.List (Sexp.Atom "[]" :: sexp_of_type contents :: lst)

  (** [sexp_of_decl (id, typ)] is the s-expression serialization of the
      Xi declaration [id: typ] *)
  let sexp_of_decl (id, typ) =
    Sexp.List [ Sexp.Atom id; sexp_of_type typ ]

  (** [sexp_of_init (id, typ) e] is the s-expression serialization of
      the initialization statement [id: typ = e] *)
  let sexp_of_init decl e =
    sexp_of_gets (sexp_of_decl decl) (sexp_of_enode e)

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

  (** [sexp_of_multi_target target] is the s-expression serialization of
      multiple initialization target [target], either a wildcard or a
      variable. *)
  let sexp_of_multi_target = function
    | MultiDecl d -> sexp_of_decl d
    | Wildcard -> Sexp.Atom "_"

  (** [sexp_of_multi_init \[e1; ...; en\] (id, args)] is the
      s-expression serialization of the multiple initialization
      statement [e1, ..., en = id(args)]. *)
  let sexp_of_multi_init targets (id, args) =
    sexp_of_gets
      (List.sexp_of_t sexp_of_multi_target targets)
      (sexp_of_call id args)

  (** [sexp_of_stmt stmt] is the s-expression serialization of [stmt] *)
  let rec sexp_of_stmt = function
    | If (e1, s1, s2) -> sexp_of_if e1 s1 s2
    | While (e, s) -> sexp_of_while e s
    | Decl decl -> sexp_of_decl decl
    | Init (decl, e) -> sexp_of_init decl e
    | Assign (target, e) -> sexp_of_assign target e
    | MultiInit (targets, call) -> sexp_of_multi_init targets call
    | ProcCall (id, args) -> sexp_of_call id args
    | Return es -> sexp_of_return es
    | ExprStmt e -> sexp_of_expr_stmt e
    | Block stmts -> sexp_of_stmts stmts

  (** [sexp_of_stmts stmts] is the s-expression serialization of
      sequenced statements [stmts] *)
  and sexp_of_stmts stmts = List.sexp_of_t sexp_of_snode stmts

  (** [sexp_of_snode node] is the s-expression serialization of the
      statement wrapped in [node] *)
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
    Sexp.List (Sexp.Atom "return" :: sexp_list_of_wrap_list es)

  (** [sexp_of_expr_stmt e] is the s-expression serialization of the
      statement [_ = e]. *)
  and sexp_of_expr_stmt : type a. a Expr.node -> Sexp.t =
   fun e -> sexp_of_gets (Sexp.Atom "_") (sexp_of_enode e)

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
    let lst = Option.to_list (Option.map init ~f:sexp_of_literal) in
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
