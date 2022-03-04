open Core
open Result.Monad_infix
open Result.Let_syntax
module Node = Node.Position
include Factory.Make (Node) (Node)
module PosNode = Node
open Expr
open Stmt
open Primitive
open Type

(** [map_error ~pos result] decorates the error variant of [result] with
    the position [pos] at which it occurs *)
let map_error ~pos = Result.map_error ~f:(Decorated.Error.make ~pos)

let type_check ast = failwith "unimplemented"

(** [type_check_expr ctx enode] is [Ok expr] where [expr] is [enode]
    decorated within context [ctx] or [Error type_error] where
    [type_error] describes a semantic error of [enode] *)
let rec type_check_expr ~(ctx : Type.context) (enode : Expr.node) :
    Decorated.expr_result =
  failwith "unimplemented"

(** [bool_or_error ctx e] is [Ok e] if [e] is [Ok e] and [e] has bool
    type in context [ctx] and [Error Mismatch] otherwise *)
let bool_or_error ~ctx expr =
  let%bind expr = type_check_expr ~ctx expr in
  let typ = Node.Expr.typ expr in
  let pos = Node.Expr.position expr in
  map_error ~pos (assert_bool typ) >>| fun () -> expr

(** [bool_or_error_stmt ctx e] is [Ok e] if [e] is [Ok e] and [e] has
    bool type in function context [ctx] and [Error Mismatch] otherwise *)
let bool_or_error_stmt ~ctx =
  bool_or_error ~ctx:(Context.Fn.context ctx)

let type_check_var_decl ~ctx pos id typ =
  let%map ctx = map_error ~pos (Context.Fn.add_var ~id ~typ ctx) in
  let s = Decorated.Stmt.VarDecl (id, typ) in
  Node.Stmt.make_unit s ~ctx ~pos

(** [type_check_stmt ctx snode] is [Ok stmt] where [stmt] is [snode]
    decorated within function context [ctx] or [Error type_error] where
    [type_error] describes a semantic error of [snode] *)
let rec type_check_stmt ~(ctx : Context.fn) (snode : Stmt.node) :
    Decorated.stmt_result =
  let pos = PosNode.position snode in
  match Stmt.Node.get snode with
  | If (e, s) -> type_check_if ~ctx ~pos e s
  | IfElse (e, s1, s2) -> type_check_if_else ~ctx ~pos e s1 s2
  | While (e, s) -> type_check_while ~ctx ~pos e s
  | VarDecl (id, typ) -> type_check_var_decl ~ctx pos id typ
  | ArrayDecl (id, typ, nodes) -> failwith "unimplemented"
  | Assign (id, node) -> failwith "unimplemented"
  | ArrAssign (n1, n2, n3) -> failwith "unimplemented"
  | ExprStmt call -> failwith "unimplemented"
  | VarInit (id, t, node) -> failwith "unimplemented"
  | MultiAssign (list, id, nodes) -> failwith "unimplemented"
  | PrCall call -> failwith "unimplemented"
  | Return nodes -> failwith "unimplemented"
  | Block block -> failwith "unimplemented"

and type_check_cond ~ctx e s =
  let%bind e = bool_or_error_stmt ~ctx e in
  let%map s = type_check_stmt ~ctx s in
  (e, s)

and make_cond ~f ~ctx ~pos e s =
  let%map e, s = type_check_cond ctx e s in
  Node.Stmt.make_unit ~ctx (f e s) ~pos

and type_check_if ~ctx ~pos e s =
  make_cond ~f:(fun e s -> Decorated.Stmt.If (e, s)) ~ctx ~pos e s

and type_check_if_else ~ctx ~pos e s1 s2 =
  let%bind e, s1 = type_check_cond ctx e s1 in
  let%map s2 = type_check_stmt ctx s2 in
  let typ = lub_stmt s1 s2 in
  let s = Decorated.Stmt.IfElse (e, s1, s2) in
  Node.Stmt.make s ~ctx ~pos ~typ

and type_check_while ~ctx ~pos e s =
  make_cond ~f:(fun e s -> Decorated.Stmt.While (e, s)) ~ctx ~pos e s
