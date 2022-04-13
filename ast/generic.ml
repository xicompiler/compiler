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
type decl = id * Type.tau

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

  (** [sexp_of_node node] is the s-expression serialization of the
      expression wrapped in node [node] *)
  and sexp_of_node node = Entry.Key.fold ~f:sexp_of_t node

  (** [sexp_of_array arr] is the s-expression serialization of the Xi
      array [arr] *)
  and sexp_of_array arr = List.sexp_of_t sexp_of_node arr

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

  (** [const_fold_bop_opt bop e1 e2] is the AST node [e1 bop e2] where
      all constant expressions have been folded *)
  let const_fold_bop_opt bop e1 e2 =
    match (e1, e2) with
    | Primitive x1, Primitive x2 ->
        prim_of_base (Binop.eval_primitive bop x1 x2)
    | Array a1, Array a2 -> arr_of_base (Binop.eval_array bop a1 a2)
    | _ -> None

  (** [const_fold_uop_opt uop e] is the AST node [uop e] where all
      constant expressions have been folded *)
  let const_fold_uop_opt uop = function
    | Primitive x -> prim_of_base (Unop.eval uop x)
    | _ -> None

  (** [const_fold e] is the AST node [e] where all constant expressions
      have been folded *)
  let rec const_fold = function
    | Array es -> const_fold_array es
    | Bop (bop, e1, e2) -> const_fold_bop bop e1 e2
    | Uop (uop, e) -> const_fold_uop uop e
    | FnCall (id, es) -> const_fold_fn_call id es
    | Length e -> const_fold_length e
    | Index (e1, e2) -> const_fold_index e1 e2
    | e -> e

  (** [const_fold_node e] is the AST node [e] where all constant
      expressions have been folded *)
  and const_fold_node e = Entry.Key.map ~f:const_fold e

  (** [const_fold_nodes es] is [es] where each expression has been
      constant folded, if possible *)
  and const_fold_nodes es = List.map ~f:const_fold_node es

  (** [const_fold_array es] is the AST node [Array es] where all
      constant expressions have been folded *)
  and const_fold_array es = Array (const_fold_nodes es)

  (** [const_fold_bop bop e1 e2] is the AST node [Bop (bop, e1, e2)]
      where all constant expressions have been folded recursive in [e1]
      and [e2], folding the resulting operation if possible. *)
  and const_fold_bop bop enode1 enode2 =
    let enode1 = const_fold_node enode1 in
    let enode2 = const_fold_node enode2 in
    let e1 = Entry.key enode1 in
    let e2 = Entry.key enode2 in
    let default () = Bop (bop, enode1, enode2) in
    Lazy.value ~default (const_fold_bop_opt bop e1 e2)

  (** [const_fold_uop uop e] is the AST node [Uop (uop, e1)] where all
      constant expressions have been folded recursive in [e], folding
      the resulting operation if possible. *)
  and const_fold_uop uop enode =
    let enode = const_fold_node enode in
    let e = Entry.key enode in
    let default () = Uop (uop, enode) in
    Lazy.value ~default (const_fold_uop_opt uop e)

  (** [const_fold_fn_call bop e1 e2] is the AST node [FnCall (id, es)]
      where all constant expressions of each expression in [es] have
      been folded *)
  and const_fold_fn_call id es = FnCall (id, const_fold_nodes es)

  (** [const_fold_length e] is the AST node [Length e] where all
      constant expressions of each expression in [e] have been folded *)
  and const_fold_length e = Length (const_fold_node e)

  (** [const_fold_index e1 e2] is the AST node [Index (e1, e2)] where
      all constant expressions of each expression in [e1] and [e2] have
      been folded *)
  and const_fold_index e1 e2 =
    Index (const_fold_node e1, const_fold_node e2)

  type ('a, 'e, 'acc) folder =
    primitive:(primitive -> 'e -> 'acc) ->
    id:(id -> 'e -> 'acc) ->
    array:('acc list -> 'e -> 'acc) ->
    string:(string -> 'e -> 'acc) ->
    bop:(binop -> 'acc -> 'acc -> 'e -> 'acc) ->
    uop:(unop -> 'acc -> 'e -> 'acc) ->
    fn_call:(id -> 'acc list -> 'e -> 'acc) ->
    length:('acc -> 'e -> 'acc) ->
    index:('acc -> 'acc -> 'e -> 'acc) ->
    'a
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
    | ExprStmt of 'e Expr.call
    | VarInit of id * Type.tau * 'e Expr.node
    | MultiAssign of decl option list * id * 'e Expr.nodes
    | PrCall of 'e Expr.call
    | Return of 'e Expr.nodes
    | Block of ('e, 's) block

  and ('e, 's) node = (('e, 's) t, 's) Entry.t
  and ('e, 's) block = ('e, 's) node list

  let empty = Block []

  (** [sexp_of_decl_type typ lengths] is the s-expression serialization
      of the array type [t\[l1\]\[l2\]...] where each length is optional *)
  let rec sexp_of_decl_type typ lengths =
    match typ with
    | #Type.Tau.primitive as p -> Type.Tau.Primitive.sexp_of_t p
    | `Array t ->
        let e, es = Util.List.hd_tl_exn lengths in
        let lst = Option.to_list (e >>| Expr.sexp_of_node) in
        Sexp.List (Sexp.Atom "[]" :: sexp_of_decl_type t es :: lst)

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
  let sexp_of_var_decl id typ = sexp_of_decl id (Type.Tau.sexp_of_t typ)

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
    sexp_of_gets (sexp_of_var_decl id typ) (Expr.sexp_of_node e)

  (** [sexp_of_multi_assign \[e1; ...; en\] id args] is the s-expression
      serialization of the multiple initialization statement
      [e1, ..., en = id(args)]. *)
  let sexp_of_multi_assign ds id args =
    let call = Expr.sexp_of_call id args in
    sexp_of_gets (List.sexp_of_t sexp_of_d ds) call

  (** [sexp_of_t stmt] is the s-expression serialization of [stmt] *)
  let rec sexp_of_t = function
    | If (e, s) -> sexp_of_if e s
    | IfElse (e, s1, s2) -> sexp_of_if_else e s1 s2
    | While (e, s) -> sexp_of_while e s
    | VarDecl (id, typ) -> sexp_of_var_decl id typ
    | ArrayDecl (id, typ, es) -> sexp_of_array_decl id typ es
    | ExprStmt (id, args) -> sexp_of_expr_stmt id args
    | VarInit (id, typ, e) -> sexp_of_init id typ e
    | Assign (id, e) -> sexp_of_var_assign id e
    | ArrAssign (e1, e2, e3) -> sexp_of_arr_assign e1 e2 e3
    | MultiAssign (ds, id, args) -> sexp_of_multi_assign ds id args
    | PrCall (id, args) -> Expr.sexp_of_call id args
    | Return es -> sexp_of_return es
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

  (** [const_fold_array_decl id typ es] is the statement
      [ArrayDecl (id, typ, es)] where each expression of [es] has been
      constant folded *)
  let const_fold_array_decl id typ es =
    let es = List.map ~f:(Option.map ~f:Expr.const_fold_node) es in
    ArrayDecl (id, typ, es)

  (** [const_fold_assign id e] is the statement [Assign (id, e)] where
      each expression of [e] has been constant folded *)
  let const_fold_assign id e = Assign (id, Expr.const_fold_node e)

  (** [const_fold_arr_assign e1 e2 e3] is the statement
      [ArrAssign (e1, e2, e3)] where each expression of [e1], [e2], and
      [e3] has been constant folded *)
  let const_fold_arr_assign e1 e2 e3 =
    let f = Expr.const_fold_node in
    let e1, e2, e3 = Tuple3.map ~f (e1, e2, e3) in
    ArrAssign (e1, e2, e3)

  (** [const_fold_expr_stmt e] is the statement [ExprStmt e] where each
      expression of [e] has been constant folded *)
  let const_fold_expr_stmt id es =
    ExprStmt (id, Expr.const_fold_nodes es)

  (** [const_fold_var_init id typ e] is the statement
      [VarInit (id, typ, e)] where each expression of [e] has been
      constant folded *)
  let const_fold_var_init id typ e =
    VarInit (id, typ, Expr.const_fold_node e)

  (** [const_fold_multi_assign ds typ es] is the statement
      [MultiAssign (ds, id, es)] where each expression of [es] has been
      constant folded *)
  let const_fold_multi_assign ds id es =
    MultiAssign (ds, id, Expr.const_fold_nodes es)

  (** [const_fold_pr_call id es] is the statement [PrCall (id, es)]
      where each expression of [es] has been constant folded *)
  let const_fold_pr_call id es = PrCall (id, Expr.const_fold_nodes es)

  (** [const_fold_return es] is the statement [Return es] where each
      expression of [es] has been constant folded *)
  let const_fold_return es = Return (Expr.const_fold_nodes es)

  (** [const_fold_cond2 ~t ~f ~default e] is [t ()] if [e] wraps a
      [true] literal, [f ()] if it wraps a [false] literal, and
      [default ()] otherwise *)
  let const_fold_cond2 ~t ~f ~default e =
    match Entry.key e with
    | Expr.Primitive (`Bool b) -> (if b then t else f) ()
    | _ -> default ()

  (** [const_fold s] is the statement [s] where each expression
      contained in [s] has been recursively constant folded *)
  let rec const_fold = function
    | If (e, s) -> const_fold_if e s
    | IfElse (e, s1, s2) -> const_fold_if_else e s1 s2
    | While (e, s) -> const_fold_while e s
    | VarDecl _ as decl -> decl
    | ArrayDecl (id, typ, es) -> const_fold_array_decl id typ es
    | Assign (id, e) -> const_fold_assign id e
    | ArrAssign (e1, e2, e3) -> const_fold_arr_assign e1 e2 e3
    | ExprStmt (id, es) -> const_fold_expr_stmt id es
    | VarInit (id, typ, e) -> const_fold_var_init id typ e
    | MultiAssign (ds, id, es) -> const_fold_multi_assign ds id es
    | PrCall (id, es) -> const_fold_pr_call id es
    | Return es -> const_fold_return es
    | Block block -> const_fold_block block

  (** [const_fold_node s] is the statement [s] where each expression
      contained in the statement pointed to by [s] has been recursively
      constant folded *)
  and const_fold_node snode = Entry.Key.map ~f:const_fold snode

  (** [const_fold_nodes block] is [block] where each statement has been
      recursively constant folded *)
  and const_fold_nodes block = List.map ~f:const_fold_node block

  (** [const_fold_if e s] is the statement [If (e, s)] where [e] and
      each expression in [s] has been constant folded *)
  and const_fold_if e s = If (Expr.const_fold_node e, const_fold_node s)

  (** [const_fold_if_else e s1 s2] is the statement [IfElse (e, s1, s2)]
      where [e] and each expression in [s1] and [s2] has been constant
      folded *)
  and const_fold_if_else e s1 s2 =
    let e' = Expr.const_fold_node e in
    IfElse (e', const_fold_node s1, const_fold_node s2)

  (** [const_fold_while e s] is the statement [If (e, s)] where [e] and
      each expression in [s] has been constant folded *)
  and const_fold_while e s =
    While (Expr.const_fold_node e, const_fold_node s)

  (** [const_fold_block stmts] is [Block stmts] where each statement in
      [stmts] has been recursively constant folded *)
  and const_fold_block block = Block (const_fold_nodes block)
end

(** [Toplevel] represents the toplevel definitions of the AST *)
module Toplevel = struct
  (** [Sig] represents a function signature in Xi *)
  module Sig = struct
    type t = {
      name : id;
      params : decl list;
      ret : Type.tau list;
    }
    [@@deriving fields]

    let create = Fields.create
  end

  type ('e, 's) fn = Sig.t * ('e, 's) Stmt.block

  (** [sexp_of_fn ?body signature] is the s-expression serialization of
      function signature [signature] with an optional function body. *)
  let sexp_of_fn ?body ({ name; params; ret } : Sig.t) =
    let id = sexp_of_id name in
    let sexp_of_param (id, typ) = Stmt.sexp_of_var_decl id typ in
    let params = List.sexp_of_t sexp_of_param params in
    let types = List.sexp_of_t Type.Tau.sexp_of_t ret in
    let body = Option.map body ~f:Stmt.sexp_of_nodes in
    Sexp.List (id :: params :: types :: Option.to_list body)

  type ('e, 's) definition =
    | FnDefn of ('e, 's) fn
    | GlobalDecl of decl
    | GlobalInit of id * Type.tau * Expr.primitive

  (** [sexp_of_global ?init id typ] is the s-expression serialization of
      the the global Xi declaration with name [id], type [typ] and
      optional initialization expression [init] *)
  let sexp_of_global ?init id typ =
    let global = Sexp.Atom ":global" in
    let id_sexp = sexp_of_id id in
    let type_sexp = Type.Tau.sexp_of_t typ in
    let lst = Option.to_list (init >>| Primitive.sexp_of_t) in
    Sexp.List (global :: id_sexp :: type_sexp :: lst)

  (** [sexp_of_defn def] is the s-expression serialization of global
      definition [def] *)
  let sexp_of_defn = function
    | FnDefn (signature, body) -> sexp_of_fn signature ~body
    | GlobalDecl (id, typ) -> sexp_of_global id typ
    | GlobalInit (id, typ, init) -> sexp_of_global id typ ~init

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

  type 't intf = (Sig.t, 't) Entry.t list

  (** [sexp_of_intf intf] is the s-expression serialization of the AST
      [intf]. *)
  let sexp_of_intf sigs = Sexp.List [ sexp_of_nodes sexp_of_fn sigs ]

  (** [const_fold_defn def] is [def] where each constituent expression
      has been recursively constant folded *)
  let const_fold_defn = function
    | FnDefn (sg, block) -> FnDefn (sg, Stmt.const_fold_nodes block)
    | global -> global

  (** [const_fold_defls defs] is [defs] where each constituent
      definition has been recursively constant folded *)
  let const_fold_defs = List.map ~f:(Entry.Key.map ~f:const_fold_defn)
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
  | Intf sigs -> sexp_of_intf sigs

let const_fold = function
  | Source src ->
      let open Source in
      let defs = const_fold_defs src.defs in
      Source { src with defs }
  | intf -> intf

let iter_source ast ~f =
  match ast with Source src -> f src | Intf _ -> ()