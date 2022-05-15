open Core
open Option.Let_syntax
open Option.Monad_infix
open Op
open Util.Option

(** [sexp_of_id id] is [Sexp.Atom id]*)
let sexp_of_id id = Sexp.Atom (Entry.key id)

(** [sexp_of_string c] is the s-expression serialization of string [s],
    surrounded by double quotes. *)
let sexp_of_string s =
  Sexp.Atom (s |> Unicode.escape_string |> Printf.sprintf "\"%s\"")

(** [sexp_of_gets lhs rhs] is the s-expression serialization of the
    statement [lhs = rhs] *)
let sexp_of_gets lhs rhs = Sexp.List [ Sexp.Atom "="; lhs; rhs ]

(** [sexp_of_use id] is the s-expression serialization of the statement
    [use id] *)
let sexp_of_use id = Sexp.List [ Sexp.Atom "use"; sexp_of_id id ]

type id = string Position.entry
type decl = id list * Type.tau

module Expr = struct
  open Op

  type primitive = Primitive.t

  type 'e t =
    | Primitive of primitive
    | Id of id
    | Array of 'e nodes
    | String of string
    | Bop of binop * 'e node * 'e node
    | Uop of unop * 'e node
    | FnCall of 'e call
    | Length of 'e node
    | Index of 'e node * 'e node
    | Field of 'e node * id
    | Null

  and 'e node = ('e t, 'e) Entry.t
  and 'e nodes = 'e node list
  and 'e call = id * 'e nodes

  (** [sexp_of_t e] is the s-expression serialization of expression [e] *)
  let rec sexp_of_t = function
    | Id id -> sexp_of_id id
    | Primitive v -> Primitive.sexp_of_t v
    | Array arr -> sexp_of_array arr
    | String s -> sexp_of_string s
    | Bop (bop, e1, e2) -> sexp_of_infix_bop bop e1 e2
    | Uop (uop, e) -> sexp_of_uop uop e
    | FnCall (id, args) -> sexp_of_call id args
    | Length e -> sexp_of_length e
    | Index (e1, e2) -> sexp_of_index e1 e2
    | Field (e1, id) -> sexp_of_field e1 id
    | Null -> sexp_of_null

  (** [sexp_of_node node] is the s-expression serialization of the
      expression wrapped in node [node] *)
  and sexp_of_node node = Entry.Key.fold ~f:sexp_of_t node

  (** [sexp_of_array arr] is the s-expression serialization of the Xi
      array [arr] *)
  and sexp_of_array arr = List.sexp_of_t sexp_of_node arr

  and sexp_of_record_expr r = Sexp.Atom (fst r)

  (** [sexp_of_infix_binop bop e1 e2] is the s-expression serialization
      of the infix binary operation represented by operation [bop] and
      expressions [e1] and [e2]. *)
  and sexp_of_infix_bop bop e1 e2 =
    sexp_of_bop (Binop.to_string bop) e1 e2

  (** [sexp_of_bop s e1 e2] is the s-expression serialization of the
      binary operation represented by operation [s] and expressions [e1]
      and [e2]. *)
  and sexp_of_bop s e1 e2 =
    Sexp.List [ Sexp.Atom s; sexp_of_node e1; sexp_of_node e2 ]

  (** [sexp_of_unop uop e] is the s-expression serialization of the
      unary operation with operator [uop] and expression [e]. *)
  and sexp_of_uop uop e =
    let uop_sexp = Sexp.Atom (Unop.to_string uop) in
    Sexp.List [ uop_sexp; sexp_of_node e ]

  (** [sexp_of_call id \[e1; ...; en\]] is the s-expression
      serialization of the application of function [id] to
      [e1, ..., en], i.e. the call [id(e1, ..., en)]. *)
  and sexp_of_call id args =
    Sexp.List
      (args |> List.map ~f:sexp_of_node |> List.cons (sexp_of_id id))

  (** [sexp_of_length e] is the s-expression serialization of the
      expression [length(e)] *)
  and sexp_of_length e =
    Sexp.List [ Sexp.Atom "length"; sexp_of_node e ]

  (** [sexp_of_index e1 e2] is the s-expression serialization of the
      indexing of array [e1] at index [e2]. *)
  and sexp_of_index e1 e2 = sexp_of_bop "[]" e1 e2

  (** [sexp_of_field e1 e2] is the s-expression serialization of getting
      the field [e2] of record [e1]. *)
  and sexp_of_field e1 id =
    Sexp.List [ Sexp.Atom "."; sexp_of_node e1; Sexp.Atom (fst id) ]

  and sexp_of_null = Sexp.Atom "null"

  (** [prim_of_base b] is [Primitive r] if [b] is [Some r] and [None]
      otherwise *)
  let prim_of_base b =
    let%map r = b in
    Primitive (r :> primitive)

  (** [arr_of_base b] is [Array r] if [b] is [Some r] and [None]
      otherwise *)
  let arr_of_base b =
    let%map r = b in
    Array r
end

module Stmt = struct
  type ('e, 's) t =
    | If of 'e Expr.node * ('e, 's) node
    | IfElse of 'e Expr.node * ('e, 's) node * ('e, 's) node
    | While of 'e Expr.node * ('e, 's) node
    | VarDecl of decl
    | ArrayDecl of id * Type.tau * 'e Expr.node option list
    | Assign of id * 'e Expr.node
    | ArrAssign of 'e Expr.node * 'e Expr.node * 'e Expr.node
    | FieldAssign of 'e Expr.node * id * 'e Expr.node
    | ExprStmt of 'e Expr.call
    | VarInit of id * Type.tau * 'e Expr.node
    | MultiAssign of (id * Type.tau) option list * id * 'e Expr.nodes
    | PrCall of 'e Expr.call
    | Return of 'e Expr.nodes
    | Break
    | Block of ('e, 's) block

  and ('e, 's) node = (('e, 's) t, 's) Entry.t
  and ('e, 's) block = ('e, 's) node list

  let empty = Block []

  (** [sexp_of_decl_type typ lengths] is the s-expression serialization
      of the array type [t\[l1\]\[l2\]...] where each length is optional *)
  let rec sexp_of_decl_type typ lengths =
    match typ with
    | #Type.Tau.primitive as p -> Type.Tau.Primitive.sexp_of_t p
    | `Record r -> Sexp.Atom r
    | `Array t ->
        let e, es = Util.List.pop_exn lengths in
        let lst = Option.to_list (e >>| Expr.sexp_of_node) in
        Sexp.List (Sexp.Atom "[]" :: sexp_of_decl_type t es :: lst)

  (** [sexp_of_decl id typ] is the s-expression serialization of the
      statement [id: typ]. *)
  let sexp_of_decl ids typ =
    Sexp.List (List.map ~f:sexp_of_id ids @ [ typ ])

  (** [sexp_of_array_decl id `Array (`Array ... (prim)) \[l1; ... lm\]]
      is the s-expression serialization of the statement
      [id: prim\[l1\]\[l2\]...]*)
  let sexp_of_array_decl id typ lengths =
    sexp_of_decl [ id ] (sexp_of_decl_type typ lengths)

  (** [sexp_of_var_decl (ids, typ)] is the s-expression serialization of
      the Xi declaration [ids: typ] *)
  let sexp_of_var_decl (ids, typ) =
    sexp_of_decl ids (Type.Tau.sexp_of_t typ)

  (** [sexp_of_assign lhs e] is the s-expression serialization of the
      assignment statement [lhs = e]. *)
  let sexp_of_assign lhs e = sexp_of_gets lhs (Expr.sexp_of_node e)

  (** [sexp_of_var_assign id e] is the s-expression serialization of the
      statement [id = e] *)
  let sexp_of_var_assign id = sexp_of_assign (sexp_of_id id)

  (** [sexp_of_arr_assign e1 e2 e3] is the s-expression serialization of
      the statement [e1\[e2\] = e3] *)
  let sexp_of_arr_assign e1 e2 =
    sexp_of_assign (Expr.sexp_of_index e1 e2)

  let sexp_of_field_assign e1 id =
    sexp_of_assign (Expr.sexp_of_field e1 id)

  (** [wildcard] is the s-expression representing a wildcard character. *)
  let wildcard = Sexp.Atom "_"

  (** [sexp_of_d d] is the s-expression serialization of initialization
      target [d], either a wildcard or a variable. *)
  let sexp_of_d = function
    | Some (id, typ) -> sexp_of_var_decl ([ id ], typ)
    | None -> wildcard

  (** [sexp_of_init id typ e] is the s-expression serialization of the
      initialization statement [id: typ = e] *)
  let sexp_of_init id typ e =
    sexp_of_gets (sexp_of_var_decl ([ id ], typ)) (Expr.sexp_of_node e)

  (** [sexp_of_multi_assign \[e1; ...; en\] id args] is the s-expression
      serialization of the multiple initialization statement
      [e1, ..., en = id(args)]. *)
  let sexp_of_multi_assign ds id args =
    let call = Expr.sexp_of_call id args in
    sexp_of_gets (List.sexp_of_t sexp_of_d ds) call

  let sexp_of_break = Sexp.Atom "break"

  (** [sexp_of_t stmt] is the s-expression serialization of [stmt] *)
  let rec sexp_of_t = function
    | If (e, s) -> sexp_of_if e s
    | IfElse (e, s1, s2) -> sexp_of_if_else e s1 s2
    | While (e, s) -> sexp_of_while e s
    | VarDecl (ids, typ) -> sexp_of_var_decl (ids, typ)
    | ArrayDecl (id, typ, es) -> sexp_of_array_decl id typ es
    | ExprStmt (id, args) -> sexp_of_expr_stmt id args
    | VarInit (id, typ, e) -> sexp_of_init id typ e
    | Assign (id, e) -> sexp_of_var_assign id e
    | ArrAssign (e1, e2, e3) -> sexp_of_arr_assign e1 e2 e3
    | FieldAssign (e1, id, e3) -> sexp_of_field_assign e1 id e3
    | MultiAssign (ds, id, args) -> sexp_of_multi_assign ds id args
    | PrCall (id, args) -> Expr.sexp_of_call id args
    | Return es -> sexp_of_return es
    | Break -> sexp_of_break
    | Block stmts -> sexp_of_nodes stmts

  (** [sexp_of_node node] is the s-expression serialization of the stmt
      wrapped in [mode]*)
  and sexp_of_node node = Entry.Key.fold ~f:sexp_of_t node

  (** [sexp_of_ts stmts] is the s-expression serialization of sequenced
      statements [stmts] *)
  and sexp_of_nodes stmts = List.sexp_of_t sexp_of_node stmts

  and sexp_of_cond e s lst =
    Sexp.List
      (Sexp.Atom "if" :: Expr.sexp_of_node e :: sexp_of_node s :: lst)

  (** [sexp_of_if e s1] is the s-expression serialization of if
      statement [if e s1] *)
  and sexp_of_if e s = sexp_of_cond e s []

  (** [sexp_of_if_else e s1 s2] is the s-expression serialization of the
      statement [if e s1 else s2] *)
  and sexp_of_if_else e s1 s2 = sexp_of_cond e s1 [ sexp_of_node s2 ]

  (** [sexp_of_while e s] is the s-expression serialzation of the
      statement [while e s] *)
  and sexp_of_while e s =
    Sexp.List [ Sexp.Atom "while"; Expr.sexp_of_node e; sexp_of_node s ]

  (** [sexp_of_return \[e1; ...; en\]] is the s-expression serialization
      of the statement [return e1, ..., en]. *)
  and sexp_of_return es =
    Sexp.List (Sexp.Atom "return" :: List.map ~f:Expr.sexp_of_node es)

  (** [sexp_of_expr_stmt id \[e1; ...; en\]] is the s-expression
      serialization of the statement [_ = id(e1, ..., en)]. *)
  and sexp_of_expr_stmt id args =
    sexp_of_gets (Sexp.Atom "_") (Expr.sexp_of_call id args)
end

(** [Toplevel] represents the toplevel definitions of the AST *)
module Toplevel = struct
  (** [Sig] represents a function signature in Xi *)
  module Sig = struct
    type t = {
      name : id;
      params : (id * Type.tau) list;
      ret : Type.tau list;
    }
    [@@deriving fields]

    let create = Fields.create
  end

  type ('e, 's) fn = Sig.t * ('e, 's) Stmt.block

  type signature =
    | FnSig of Sig.t
    | RecordSig of id * decl list

  type 't intf = {
    uses : (id, 't) Entry.t list;
    sigs : (signature, 't) Entry.t list;
  }

  (** [sexp_of_fn ?body signature] is the s-expression serialization of
      function signature [signature] with an optional function body. *)
  let sexp_of_fn ?body ({ name; params; ret } : Sig.t) =
    let id = sexp_of_id name in
    let sexp_of_param (id, typ) = Stmt.sexp_of_var_decl ([ id ], typ) in
    let params = List.sexp_of_t sexp_of_param params in
    let types = List.sexp_of_t Type.Tau.sexp_of_t ret in
    let body = Option.map body ~f:Stmt.sexp_of_nodes in
    Sexp.List (id :: params :: types :: Option.to_list body)

  let sexp_of_record id (decls : decl list) =
    let id = sexp_of_id id in
    let decls = List.sexp_of_t Stmt.sexp_of_var_decl decls in
    Sexp.List [ id; decls ]

  let sexp_of_sig = function
    | FnSig f -> sexp_of_fn f
    | RecordSig (id, decls) -> sexp_of_record id decls

  type ('e, 's) definition =
    | FnDefn of ('e, 's) fn
    | RecordDefn of id * decl list
    | GlobalDecl of decl
    | GlobalInit of id * Type.tau * Expr.primitive

  (** [sexp_of_global ?init id typ] is the s-expression serialization of
      the the global Xi declaration with name [id], type [typ] and
      optional initialization expression [init] *)
  let sexp_of_global ?init ids typ =
    let global = Sexp.Atom ":global" in
    let id_sexps = List.map ~f:sexp_of_id ids in
    let type_sexp = Type.Tau.sexp_of_t typ in
    let lst = Option.to_list (init >>| Primitive.sexp_of_t) in
    Sexp.List ((global :: id_sexps) @ (type_sexp :: lst))

  (** [sexp_of_defn def] is the s-expression serialization of global
      definition [def] *)
  let sexp_of_defn = function
    | FnDefn (signature, body) -> sexp_of_fn signature ~body
    | RecordDefn (id, decls) -> sexp_of_record id decls
    | GlobalDecl (ids, typ) -> sexp_of_global ids typ
    | GlobalInit (id, typ, init) -> sexp_of_global [ id ] typ ~init

  type ('e, 's, 't) node = (('e, 's) definition, 't) Entry.t

  (** [sexp_of_nodes f lst] is the s-expression serialization of
      toplevel node list [lst] where each element is serialized using
      [f] *)
  let sexp_of_nodes f = List.sexp_of_t (Entry.Key.fold ~f)

  (** [Source] represents a source file in Xi *)
  module Source = struct
    type 't use = (id, 't) Entry.t

    type ('e, 's, 't) t = {
      uses : 't use list;
      defs : ('e, 's, 't) node list;
    }
    [@@deriving fields]

    let create = Fields.create

    (** [sexp_of_t source] is the s-expression serialization of the AST
        [source] *)
    let sexp_of_t { uses; defs } =
      let uses = sexp_of_nodes sexp_of_use uses in
      let defs = sexp_of_nodes sexp_of_defn defs in
      Sexp.List [ uses; defs ]
  end

  (** [sexp_of_intf intf] is the s-expression serialization of the AST
      [intf]. *)
  let sexp_of_intf { uses; sigs } =
    Sexp.List [ sexp_of_nodes sexp_of_sig sigs ]
end

(** An expression of type [t] is an expression representing a node of
    the Abstract Syntax Tree of a Xi program, described either by a
    source or intf file. *)
type ('e, 's, 't) t =
  | Source of ('e, 's, 't) Toplevel.Source.t
  | Intf of 't Toplevel.intf
[@@deriving variants]

open Toplevel

let sexp_of_t = function
  | Source s -> Source.sexp_of_t s
  | Intf intf -> sexp_of_intf intf

let iter_source ast ~f =
  match ast with Source src -> f src | Intf _ -> ()
