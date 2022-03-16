open Core
open Int64
open Context
open Node
open Option.Let_syntax
open Option.Monad_infix

include
  Factory.Make (Decorated.Expr) (Decorated.Stmt) (Decorated.Toplevel)

module Error = Type.Error.Positioned

type expr_result = Expr.node Error.result
type stmt_result = Stmt.node Error.result
type nonrec result = t Error.result

open Expr
open Stmt
open Toplevel

let div_opt i1 i2 = Option.try_with (fun () -> i1 / i2)

let rec const_fold_expr = function
  | Array es -> const_fold_array es
  | Bop (bop, e1, e2) -> const_fold_bop bop e1 e2
  | Uop (uop, e) -> const_fold_uop uop e
  | FnCall (id, es) -> const_fold_fn_call id es
  | Length e -> const_fold_length e
  | Index (e1, e2) -> const_fold_index e1 e2
  | e -> e

and const_fold_enode e = Expr.Node.map ~f:const_fold_expr e
and const_fold_array es = Array (const_fold_enodes es)

and const_fold_bop bop enode1 enode2 =
  let enode1 = const_fold_enode enode1 in
  let enode2 = const_fold_enode enode2 in
  match (Expr.Node.get enode1, Expr.Node.get enode2) with
  | Primitive (Int i1), Primitive (Int i2) -> fold_int_bop bop i1 i2
  | Primitive (Bool b1), Primitive (Bool b2) -> fold_bool_bop bop b1 b2
  | _ -> Bop (bop, enode1, enode2)

and const_fold_bop_opt bop e1 e2 =
  match (e1, e2) with
  | Primitive (Int i1), Primitive (Int i2) -> fold_int_bop bop i1 i2
  | Primitive (Bool b1), Primitive (Bool b2) -> fold_bool_bop bop b1 b2
  | _ -> None

and fold_int_bop bop i1 i2 =
  match bop with
  | Mult -> Some (Int i1 * i2)
  | HighMult -> Some (Int (high_mult i1 i2))
  | Div -> div_opt i1 i2 >>| Primitive.int
  | Mod -> mod_opt i1 i2 >>| Primitive.int
  |


and const_fold_enodes es = Util.List.map_tr ~f:const_fold_enode

let const_fold_if e s = If (const_fold_enode e, s)
let const_fold_if_else e s1 s2 = IfElse (const_fold_enode e, s1, s2)
let const_fold_while e s = While (const_fold_enode e, s)

let const_fold_array_decl id typ es =
  ArrayDecl (id, typ, const_fold_enodes es)

let const_fold_assign id e = Assign (id, const_fold_enode e)

let const_fold_arr_assign e1 e2 e3 =
  let e1, e2, e3 = Tuple3.map ~f:const_fold_enode (e1, e2, e3) in
  ArrAssign (e1, e2, e3)

let const_fold_expr_stmt id es = ExprStmt (id, const_fold_enodes es)
let const_fold_var_init id typ e = VarInit (id, typ, const_fold_enode e)

let const_fold_multi_assign ds id es =
  MultiAssign (ds, id, const_fold_enodes es)

let const_fold_pr_call id es = PrCall (id, const_fold_enodes es)
let const_fold_return es = Return (const_fold_enodes es)

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
  | Block block -> Block (const_fold_block block)

and const_fold_block block =
  Util.List.map_tr ~f:(Stmt.Node.map ~f:const_fold_stmt) block

let const_fold_defn = function
  | FnDefn (proto, block) -> FnDefn (proto, const_fold_block block)
  | global -> global

let const_fold_defs =
  Util.List.map_tr ~f:(Toplevel.Node.map ~f:const_fold_defn)

let const_fold = function
  | Source src ->
      let definitions = const_fold_defs src.definitions in
      Source { src with definitions }
  | intf -> intf
