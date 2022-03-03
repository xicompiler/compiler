open Core
open Result.Monad_infix
open Result.Let_syntax
module Node = Node.Position
include Factory.Make (Node) (Node)
open Expr
open Stmt
open Type
open Primitive

let type_check ast = failwith "unimplemented"

(** [type_check_expr ctx enode] is [Ok expr] where [expr] is [enode]
    decorated within context [ctx] or [Error type_error] where
    [type_error] describes a semantic error of [enode] *)
let rec type_check_expr (ctx : Type.context) (enode : Expr.node) :
    Decorated.Expr.node Type.result =
  failwith "unimplemented"

(** [bool_or_error ctx e] is [Ok e] if [e] is [Ok e] and [e] has bool
    type in context [ctx] and [Error Mismatch] otherwise *)
let bool_or_error ctx e =
  let%bind e = type_check_expr ctx e in
  assert_bool (Decorated.Ex.typ e) >>| fun () -> e

(** [bool_or_error_stmt ctx e] is [Ok e] if [e] is [Ok e] and [e] has
    bool type in function context [ctx] and [Error Mismatch] otherwise *)
let bool_or_error_stmt ctx = bool_or_error (Context.Fn.context ctx)

(** [lub_stmt s1 s2] is [lub t1 t2] if [s1] has type [t1] and [s2] has
    type [t2] *)
let lub_stmt s1 s2 = lub (Decorated.St.typ s1) (Decorated.St.typ s2)

let type_check_var_decl ctx id typ =
  let%map ctx = Context.Fn.add_var ~id ~typ ctx in
  let s = Decorated.Stmt.VarDecl (id, typ) in
  Decorated.St.make_unit s ~ctx

(** [type_check_stmt ctx snode] is [Ok stmt] where [stmt] is [snode]
    decorated within function context [ctx] or [Error type_error] where
    [type_error] describes a semantic error of [snode] *)
let rec type_check_stmt (ctx : Context.fn) (snode : Stmt.node) :
    Decorated.Stmt.node Type.result =
  match Stmt.Node.get snode with
  | If (e, s) -> type_check_if ctx e s
  | IfElse (e, s1, s2) -> type_check_if_else ctx e s1 s2
  | While (e, s) -> type_check_while ctx e s
  | VarDecl (id, typ) -> type_check_var_decl ctx id typ
  | ArrayDecl (id, typ, nodes) -> failwith "unimplemented"
  | Assign (id, node) -> failwith "unimplemented"
  | ArrAssign (n1, n2, n3) -> failwith "unimplemented"
  | ExprStmt call -> failwith "unimplemented"
  | VarInit (id, t, node) -> failwith "unimplemented"
  | MultiAssign (list, id, nodes) -> failwith "unimplemented"
  | PrCall call -> failwith "unimplemented"
  | Return nodes -> failwith "unimplemented"
  | Block block -> failwith "unimplemented"

and type_check_cond ctx e s =
  let%bind e = bool_or_error_stmt ctx e in
  let%map s = type_check_stmt ctx s in
  (e, s)

and make_cond ~f ctx e s =
  let%map e, s = type_check_cond ctx e s in
  Decorated.St.make_unit ~ctx (f e s)

and type_check_if ctx e s =
  make_cond ~f:(fun e s -> Decorated.Stmt.If (e, s)) ctx e s

and type_check_if_else ctx e s1 s2 =
  let%bind e, s1 = type_check_cond ctx e s1 in
  let%map s2 = type_check_stmt ctx s2 in
  let typ = lub_stmt s1 s2 in
  let s = Decorated.Stmt.IfElse (e, s1, s2) in
  Decorated.St.make s ~ctx ~typ

and type_check_while ctx e s =
  make_cond ~f:(fun e s -> Decorated.Stmt.While (e, s)) ctx e s
