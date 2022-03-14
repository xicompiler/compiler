open Core
open Context
open Node
open Option.Let_syntax

include
  Factory.Make (Decorated.Expr) (Decorated.Stmt) (Decorated.Toplevel)

module Error = Type.Error.Positioned

type expr_result = Expr.node Error.result
type stmt_result = Stmt.node Error.result
type nonrec result = t Error.result

open Expr
open Stmt
open Toplevel

let const_fold_expr e = failwith "unimplemented"

let const_fold_enode (e : Expr.node) : Expr.node =
  failwith "unimplemented"

let const_fold_cond b s = if b then Some s else None
let logical_neg e = Expr.Node.set ~value:(Uop (LogicalNeg, e)) e

let const_fold_branches e s1 s2 =
  match (s1 (), s2 ()) with
  | None, None -> None
  | Some s1, None -> Some (If (e, s1))
  | None, Some s2 -> Some (If (logical_neg e, s2))
  | Some s1, Some s2 -> Some (IfElse (e, s1, s2))

let rec const_fold_stmt (node : Stmt.node) : Stmt.node option =
  match Stmt.Node.get node with
  | If (e, s) -> const_fold_if ~node e s
  | IfElse (e, s1, s2) -> const_fold_if_else ~node e s1 s2
  | While (e, s) -> const_fold_while e s
  | VarDecl _ as decl -> decl
  | ArrayDecl (id, typ, es) -> const_fold_array_decl id typ es
  | Assign (id, e) -> const_fold_assign id e
  | ArrAssign (e1, e2, e3) -> const_fold_arr_assign e1 e2 e3
  | ExprStmt (id, es) -> const_fold_expr_stmt id es
  | VarInit (id, typ, es) -> const_fold_var_init id typ es
  | MultiAssign (ds, id, es) -> const_fold_multi_assign ds id es
  | PrCall (id, es) -> const_fold_pr_call id es
  | Return es -> const_fold_return es
  | Block block -> const_fold_block block

and const_fold_if ~node enode snode =
  let%bind s = const_fold_stmt snode in
  let enode = const_fold_expr enode in
  match Expr.Node.get enode with
  | Primitive (Bool b) -> const_fold_cond b s
  | _ -> Some (Stmt.Node.set ~value:(If (enode, s)) node)

and const_fold_if_else ~node e snode1 snode2 =
  let e = const_fold_expr e in
  let snode1 () = const_fold_stmt snode1 in
  let snode2 () = const_fold_stmt snode2 in
  match Expr.Node.get e with
  | Primitive (Bool b) -> (if b then snode1 else snode2) ()
  | _ ->
      let%map value = const_fold_branches e snode1 snode2 in
      Stmt.Node.set ~value node

and const_fold_while ~node e s =
  let s = const_fold_stmt s in
  match const_fold_expr e with
  | Primitive (Bool false) -> s
  | e ->
    let default = Block [] in 
    let value = While (e, Option.value ~default:)
    Some (Stmt.Node.set ~value:(If (e, s))
  

and const_fold_stmts =
  Util.List.map_tr ~f:(Stmt.Node.map const_fold_stmt)

let const_fold_defn = function
  | FnDefn (proto, block) -> FnDefn (proto, const_fold_stmts block)
  | global -> global

let const_fold_defs =
  Util.List.map_tr ~f:(Toplevel.Node.map ~f:const_fold_defn)

let const_fold = function
  | Source src ->
      let definitions = const_fold_defs src.definitions in
      Source { src with definitions }
  | intf -> intf
