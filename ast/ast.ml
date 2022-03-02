open Core
open Result.Monad_infix
open Result.Let_syntax
include Factory.Make (Node.Pos) (Node.Pos)
open Expr
open Stmt
open Type

let type_check ast = failwith "unimplemented"

let type_check_enode (ctx : Type.context) (e : Expr.node) :
    Decorated.Expr.node Type.result =
  failwith "unimplemented"

(** [lub t1 t2] is [`Void] if both of [t1] and [t2] are [`Void] and
    [`Unit] otherwise. *)
let lub t1 t2 =
  match (t1, t2) with
  | `Void, `Void -> `Void
  | _ -> `Unit

(** [bool_or_error e] is [Ok ()] if [e] has bool type and
    [Error Mismatch] otherwise *)
let bool_or_error e = e |> Decorated.Ex.typ |> assert_bool

(** [type_check_stmt fn_ctx s] is the type of [s] within function
    context [fn_ctx] *)
let rec type_check_stmt fn_ctx = function
  | If (e, s) -> type_check_if fn_ctx e s
  | IfElse (e, s1, s2) -> failwith "unimplemented"
  | While (e, s) -> failwith "unimplemented"
  | VarDecl decl -> failwith "unimplemented"
  | ArrayDecl (id, typ, nodes) -> failwith "unimplemented"
  | Assign (id, node) -> failwith "unimplemented"
  | ArrAssign (n1, n2, n3) -> failwith "unimplemented"
  | ExprStmt call -> failwith "unimplemented"
  | VarInit (id, t, node) -> failwith "unimplemented"
  | MultiAssign (list, id, nodes) -> failwith "unimplemented"
  | PrCall call -> failwith "unimplemented"
  | Return nodes -> failwith "unimplemented"
  | Block block -> failwith "unimplemented"

(** [type_check_snode fn_ctx s] is the type of statement node [s] within
    function context [fn_ctx] *)
and type_check_snode fn_ctx snode =
  snode |> Stmt.Node.get |> type_check_stmt fn_ctx

and type_check_if fn_ctx e s =
  let ctx = Context.Fn.context fn_ctx in
  let%bind e = type_check_enode ctx e in
  let%bind () = bool_or_error e in
  let%map s = type_check_snode fn_ctx s in
  let s = Decorated.Stmt.If (e, s) in
  Decorated.St.make s ~ctx ~typ:`Unit

and type_check_while fn_ctx e s =
  let ctx = Context.Fn.context fn_ctx in
  let%bind e = type_check_enode ctx e in
  let%bind () = bool_or_error e in
  let%map s = type_check_stmt fn_ctx s in
  let s' = Decorated.Stmt.While (e, s) in
  Decorated.St.make s' ~ctx ~typ:`Unit