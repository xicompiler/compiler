open Core
open Option.Let_syntax
open Abstract
open Op

module Make (Ex : Node.S) (St : Node.S) (Tp : Node.S) = struct
  module Expr = struct
    type primitive = Primitive.t

    module Node = Ex

    type t =
      | Primitive of primitive
      | Id of id
      | Array of node list
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

  module Toplevel = struct
    module Node = Tp

    type fn = signature * Stmt.block

    type definition =
      | FnDefn of fn
      | GlobalDecl of decl
      | GlobalInit of id * Type.tau * Expr.primitive

    type node = definition Node.t

    type source = {
      uses : id Node.t list;
      definitions : node list;
    }

    type intf = signature Node.t list
  end

  type t =
    | Source of Toplevel.source
    | Intf of Toplevel.intf
  [@@deriving variants]

  (** [sexp_of_id id] is [Sexp.Atom id]*)
  let sexp_of_id id = Sexp.Atom (Node.Position.get id)

  open Expr
  open Stmt
  open Toplevel

  let int_bound = "9223372036854775808"

  (** [sexp_of_gets lhs rhs] is the s-expression serialization of the
      statement [lhs = rhs] *)
  let sexp_of_gets lhs rhs = Sexp.List [ Sexp.Atom "="; lhs; rhs ]

  (** [sexp_of_use id] is the s-expression serialization of the
      statement [use id] *)
  let sexp_of_use id = Sexp.List [ Sexp.Atom "use"; sexp_of_id id ]

  (** [sexp_of_string c] is the s-expression serialization of string
      [s], surrounded by double quotes. *)
  let sexp_of_string s =
    Sexp.Atom (s |> Unicode.escape_string |> Printf.sprintf "\"%s\"")

  (** [sexp_of_expr e] is the s-expression serialization of expression
      [e] *)
  let rec sexp_of_expr = function
    | Id id -> sexp_of_id id
    | Primitive v -> Primitive.sexp_of_t v
    | Array arr -> sexp_of_array arr
    | String s -> sexp_of_string s
    | Bop (bop, e1, e2) -> sexp_of_infix_bop bop e1 e2
    | Uop (uop, e) -> sexp_of_uop uop e
    | FnCall (id, args) -> sexp_of_call id args
    | Length e -> sexp_of_length e
    | Index (e1, e2) -> sexp_of_index e1 e2

  (** [sexp_of_enode node] is the s-expression serialization of the
      expression wrapped in node [node] *)
  and sexp_of_enode node = node |> Ex.get |> sexp_of_expr

  (** [sexp_of_array arr] is the s-expression serialization of the Xi
      array [arr] *)
  and sexp_of_array arr = List.sexp_of_t sexp_of_enode arr

  (** [sexp_of_infix_binop bop e1 e2] is the s-expression serialization
      of the infix binary operation represented by operation [bop] and
      expressions [e1] and [e2]. *)
  and sexp_of_infix_bop bop e1 e2 =
    sexp_of_bop (Binop.to_string bop) e1 e2

  (** [sexp_of_bop s e1 e2] is the s-expression serialization of the
      binary operation represented by operation [s] and expressions [e1]
      and [e2]. *)
  and sexp_of_bop s e1 e2 =
    Sexp.List [ Sexp.Atom s; sexp_of_enode e1; sexp_of_enode e2 ]

  (** [sexp_of_unop uop e] is the s-expression serialization of the
      unary operation with operator [uop] and expression [e]. *)
  and sexp_of_uop uop e =
    let uop_sexp = Sexp.Atom (Unop.to_string uop) in
    Sexp.List [ uop_sexp; sexp_of_enode e ]

  (** [sexp_of_call id \[e1; ...; en\]] is the s-expression
      serialization of the application of function [id] to
      [e1, ..., en], i.e. the call [id(e1, ..., en)]. *)
  and sexp_of_call id args =
    Sexp.List
      (args |> List.map ~f:sexp_of_enode |> List.cons (sexp_of_id id))

  (** [sexp_of_length e] is the s-expression serialization of the
      expression [length(e)] *)
  and sexp_of_length e =
    Sexp.List [ Sexp.Atom "length"; sexp_of_enode e ]

  (** [sexp_of_index e1 e2] is the s-expression serialization of the
      indexing of array [e1] at index [e2]. *)
  and sexp_of_index e1 e2 = sexp_of_bop "[]" e1 e2

  (** [sexp_of_prim_type t] is the s-expression serialization of
      primitive type [t] *)
  let sexp_of_prim_type = function
    | `Int -> Sexp.Atom "int"
    | `Bool -> Sexp.Atom "bool"
    | `Poly -> Sexp.Atom "<poly>"

  (** [sexp_of_type t] is the s-expression serialization of type [t] *)
  let rec sexp_of_type = function
    | `Array t -> Sexp.List [ Sexp.Atom "[]"; sexp_of_type t ]
    | (`Int | `Bool | `Poly) as prim -> sexp_of_prim_type prim

  (** [sexp_of_decl_type typ lengths] is the s-expression serialization
      of the array type [t\[l1\]\[l2\]...] where each length is optional *)
  let rec sexp_of_decl_type typ lengths =
    match typ with
    | `Array t ->
        let e, es = Util.List.hd_tl_exn lengths in
        let lst = Option.to_list (Option.map e ~f:sexp_of_enode) in
        Sexp.List (Sexp.Atom "[]" :: sexp_of_decl_type t es :: lst)
    | (`Int | `Bool | `Poly) as prim -> sexp_of_prim_type prim

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
  and sexp_of_snode node = node |> St.get |> sexp_of_stmt

  and sexp_of_cond e s lst =
    Sexp.List
      (Sexp.Atom "if" :: sexp_of_enode e :: sexp_of_snode s :: lst)

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
    let lst = Option.to_list (Option.map init ~f:Primitive.sexp_of_t) in
    Sexp.List (global :: id_sexp :: type_sexp :: lst)

  (** [sexp_of_definition def] is the s-expression serialization of
      global definition [def] *)
  let sexp_of_definition = function
    | FnDefn (signature, body) -> sexp_of_fn signature ~body
    | GlobalDecl (id, typ) -> sexp_of_global id typ
    | GlobalInit (id, typ, init) -> sexp_of_global id typ ~init

  (** [sexp_of_tp_list f lst] is the s-expression serialization of
      toplevel node list [lst] where each element is serialized using
      [f] *)
  let sexp_of_tp_list f = List.sexp_of_t (fun n -> f (Tp.get n))

  (** [sexp_of_source source] is the s-expression serialization of the
      AST [source] *)
  let sexp_of_source { uses; definitions } =
    Sexp.List
      [
        sexp_of_tp_list sexp_of_use uses;
        sexp_of_tp_list sexp_of_definition definitions;
      ]

  (** [sexp_of_intf intf] is the s-expression serialization of the AST
      [intf]. *)
  let sexp_of_intf sigs = Sexp.List [ sexp_of_tp_list sexp_of_fn sigs ]

  let sexp_of_t = function
    | Source s -> sexp_of_source s
    | Intf sigs -> sexp_of_intf sigs

  (** [prim_of_base b] is [Primitive r] if [b] is [Some r] and [None]
      otherwise *)
  let prim_of_base b =
    let%map r = b in
    Primitive (r :> primitive)

  (** [const_fold_bop_opt bop e1 e2] is the AST node [e1 bop e2] where
      all constant expressions have been folded *)
  let const_fold_bop_opt bop e1 e2 =
    match (e1, e2) with
    | Primitive x1, Primitive x2 -> prim_of_base (Binop.eval bop x1 x2)
    | _ -> None

  (** [const_fold_uop_opt uop e] is the AST node [uop e] where all
      constant expressions have been folded *)
  let const_fold_uop_opt uop = function
    | Primitive x -> prim_of_base (Unop.eval uop x)
    | _ -> None

  (** [const_fold_expr e] is the AST node [e] where all constant
      expressions have been folded *)
  let rec const_fold_expr = function
    | Array es -> const_fold_array es
    | Bop (bop, e1, e2) -> const_fold_bop bop e1 e2
    | Uop (uop, e) -> const_fold_uop uop e
    | FnCall (id, es) -> const_fold_fn_call id es
    | Length e -> const_fold_length e
    | Index (e1, e2) -> const_fold_index e1 e2
    | e -> e

  (** [const_fold_enode e] is the AST node [e] where all constant
      expressions have been folded *)
  and const_fold_enode e = Expr.Node.map ~f:const_fold_expr e

  (** [const_fold_array es] is the AST node [Array es] where all
      constant expressions have been folded *)
  and const_fold_array es = Array (const_fold_enodes es)

  (** [const_fold_bop bop e1 e2] is the AST node [Bop (bop, e1, e2)]
      where all constant expressions have been folded recursive in [e1]
      and [e2], folding the resulting operation if possible. *)
  and const_fold_bop bop enode1 enode2 =
    let enode1 = const_fold_enode enode1 in
    let enode2 = const_fold_enode enode2 in
    let e1 = Expr.Node.get enode1 in
    let e2 = Expr.Node.get enode2 in
    let default = Bop (bop, enode1, enode2) in
    Option.value ~default (const_fold_bop_opt bop e1 e2)

  (** [const_fold_uop uop e] is the AST node [Uop (uop, e1)] where all
      constant expressions have been folded recursive in [e], folding
      the resulting operation if possible. *)
  and const_fold_uop uop enode =
    let enode = const_fold_enode enode in
    let e = Expr.Node.get enode in
    let default = Uop (uop, enode) in
    Option.value ~default (const_fold_uop_opt uop e)

  (** [const_fold_fn_call bop e1 e2] is the AST node [FnCall (id, es)]
      where all constant expressions of each expression in [es] have
      been folded *)
  and const_fold_fn_call id es = FnCall (id, const_fold_enodes es)

  (** [const_fold_length e] is the AST node [Length e] where all
      constant expressions of each expression in [e] have been folded *)
  and const_fold_length e = Length (const_fold_enode e)

  (** [const_fold_index e1 e2] is the AST node [Index (e1, e2)] where
      all constant expressions of each expression in [e1] and [e2] have
      been folded *)
  and const_fold_index e1 e2 =
    Index (const_fold_enode e1, const_fold_enode e2)

  (** [const_fold_enodes es] is [es] where each expression has been
      constant folded, if possible *)
  and const_fold_enodes es = List.map ~f:const_fold_enode es

  (** [const_fold_array_decl id typ es] is the statement
      [ArrayDecl (id, typ, es)] where each expression of [es] has been
      constant folded *)
  let const_fold_array_decl id typ es =
    let es = List.map ~f:(Option.map ~f:const_fold_enode) es in
    ArrayDecl (id, typ, es)

  (** [const_fold_assign id e] is the statement [Assign (id, e)] where
      each expression of [e] has been constant folded *)
  let const_fold_assign id e = Assign (id, const_fold_enode e)

  (** [const_fold_arr_assign e1 e2 e3] is the statement
      [ArrAssign (e1, e2, e3)] where each expression of [e1], [e2], and
      [e3] has been constant folded *)
  let const_fold_arr_assign e1 e2 e3 =
    let e1, e2, e3 = Tuple3.map ~f:const_fold_enode (e1, e2, e3) in
    ArrAssign (e1, e2, e3)

  (** [const_fold_expr_stmt e] is the statement [ExprStmt e] where each
      expression of [e] has been constant folded *)
  let const_fold_expr_stmt id es = ExprStmt (id, const_fold_enodes es)

  (** [const_fold_var_init id typ e] is the statement
      [VarInit (id, typ, e)] where each expression of [e] has been
      constant folded *)
  let const_fold_var_init id typ e =
    VarInit (id, typ, const_fold_enode e)

  (** [const_fold_multi_assign ds typ es] is the statement
      [MultiAssign (ds, id, es)] where each expression of [es] has been
      constant folded *)
  let const_fold_multi_assign ds id es =
    MultiAssign (ds, id, const_fold_enodes es)

  (** [const_fold_pr_call id es] is the statement [PrCall (id, es)]
      where each expression of [es] has been constant folded *)
  let const_fold_pr_call id es = PrCall (id, const_fold_enodes es)

  (** [const_fold_return es] is the statement [Return es] where each
      expression of [es] has been constant folded *)
  let const_fold_return es = Return (const_fold_enodes es)

  (** [const_fold_stmt s] is the statement [s] where each expression
      contained in [s] has been recursively constant folded *)
  let rec const_fold_stmt = function
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

  (** [const_fold_snode s] is the statement [s] where each expression
      contained in the statement pointed to by [s] has been recursively
      constant folded *)
  and const_fold_snode snode = Stmt.Node.map ~f:const_fold_stmt snode

  (** [const_fold_snodes stmts] is [stmts] where each statement has been
      recursively constant folded *)
  and const_fold_snodes block = List.map ~f:const_fold_snode block

  (** [const_fold_if e s] is the statement [If (e, s)] where [e] and
      each expression in [s] has been constant folded *)
  and const_fold_if e s = If (const_fold_enode e, const_fold_snode s)

  (** [const_fold_if_else e s1 s2] is the statement [IfElse (e, s1, s2)]
      where [e] and each expression in [s1] and [s2] has been constant
      folded *)
  and const_fold_if_else e s1 s2 =
    IfElse (const_fold_enode e, const_fold_snode s1, const_fold_snode s2)

  (** [const_fold_while e s] is the statement [If (e, s)] where [e] and
      each expression in [s] has been constant folded *)
  and const_fold_while e s =
    While (const_fold_enode e, const_fold_snode s)

  (** [const_fold_block stmts] is [Block stmts] where each statement in
      [stmts] has been recursively constant folded *)
  and const_fold_block block = Block (const_fold_snodes block)

  (** [const_fold_defn def] is [def] where each constituent expression
      has been recursively constant folded *)
  let const_fold_defn = function
    | FnDefn (proto, block) -> FnDefn (proto, const_fold_snodes block)
    | global -> global

  (** [const_fold_defls defs] is [defs] where each constituent
      definition has been recursively constant folded *)
  let const_fold_defs =
    List.map ~f:(Toplevel.Node.map ~f:const_fold_defn)

  let const_fold = function
    | Source src ->
        let definitions = const_fold_defs src.definitions in
        Source { src with definitions }
    | intf -> intf
end
