open Core
open Int64
open Context
open Node
open Option.Let_syntax
open Option.Monad_infix
open Op

include
  Factory.Make (Decorated.Expr) (Decorated.Stmt) (Decorated.Toplevel)

module Error = Type.Error.Positioned

type expr_result = Expr.node Error.result
type stmt_result = Stmt.node Error.result
type nonrec result = t Error.result

open Expr
open Stmt
open Toplevel

(** [prim_of_base b] is [Primitive r] if [b] is [Some r] and [None]
    otherwise *)
let prim_of_base b =
  let%map r = b in
  Primitive (r :> primitive)

(** [const_fold_bop_opt bop e1 e2] is the AST node [e1 bop e2] where all
    constant expressions have been folded *)
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

(** [const_fold_array es] is the AST node [Array es] where all constant
    expressions have been folded *)
and const_fold_array es = Array (const_fold_enodes es)

(** [const_fold_bop bop e1 e2] is the AST node [Bop (bop, e1, e2)] where
    all constant expressions have been folded recursive in [e1] and
    [e2], folding the resulting operation if possible. *)
and const_fold_bop bop enode1 enode2 =
  let enode1 = const_fold_enode enode1 in
  let enode2 = const_fold_enode enode2 in
  let e1 = Expr.Node.get enode1 in
  let e2 = Expr.Node.get enode2 in
  let default = Bop (bop, enode1, enode2) in
  Option.value ~default (const_fold_bop_opt bop e1 e2)

(** [const_fold_uop uop e] is the AST node [Uop (uop, e1)] where all
    constant expressions have been folded recursive in [e], folding the
    resulting operation if possible. *)
and const_fold_uop uop enode =
  let enode = const_fold_enode enode in
  let e = Expr.Node.get enode in
  let default = Uop (uop, enode) in
  Option.value ~default (const_fold_uop_opt uop e)

(** [const_fold_fn_call bop e1 e2] is the AST node [FnCall (id, es)]
    where all constant expressions of each expression in [es] have been
    folded *)
and const_fold_fn_call id es = FnCall (id, const_fold_enodes es)

(** [const_fold_length e] is the AST node [Length e] where all constant
    expressions of each expression in [e] have been folded *)
and const_fold_length e = Length (const_fold_enode e)

(** [const_fold_index e1 e2] is the AST node [Index (e1, e2)] where all
    constant expressions of each expression in [e1] and [e2] have been
    folded *)
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
let const_fold_var_init id typ e = VarInit (id, typ, const_fold_enode e)

(** [const_fold_multi_assign ds typ es] is the statement
    [MultiAssign (ds, id, es)] where each expression of [es] has been
    constant folded *)
let const_fold_multi_assign ds id es =
  MultiAssign (ds, id, const_fold_enodes es)

(** [const_fold_pr_call id es] is the statement [PrCall (id, es)] where
    each expression of [es] has been constant folded *)
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

(** [const_fold_if e s] is the statement [If (e, s)] where [e] and each
    expression in [s] has been constant folded *)
and const_fold_if e s = If (const_fold_enode e, const_fold_snode s)

(** [const_fold_if_else e s1 s2] is the statement [IfElse (e, s1, s2)]
    where [e] and each expression in [s1] and [s2] has been constant
    folded *)
and const_fold_if_else e s1 s2 =
  IfElse (const_fold_enode e, const_fold_snode s1, const_fold_snode s2)

(** [const_fold_while e s] is the statement [If (e, s)] where [e] and
    each expression in [s] has been constant folded *)
and const_fold_while e s = While (const_fold_enode e, const_fold_snode s)

(** [const_fold_block stmts] is [Block stmts] where each statement in
    [stmts] has been recursively constant folded *)
and const_fold_block block = Block (const_fold_snodes block)

(** [const_fold_defn def] is [def] where each constituent expression has
    been recursively constant folded *)
let const_fold_defn = function
  | FnDefn (proto, block) -> FnDefn (proto, const_fold_snodes block)
  | global -> global

(** [const_fold_defls defs] is [defs] where each constituent definition
    has been recursively constant folded *)
let const_fold_defs = List.map ~f:(Toplevel.Node.map ~f:const_fold_defn)

let const_fold = function
  | Source src ->
      let definitions = const_fold_defs src.definitions in
      Source { src with definitions }
  | intf -> intf
