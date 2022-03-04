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

(** [type_of_primitive p] is [`Int] if [p] is [Int] or [Char] and
    [`Bool] if [p] is [Bool] *)
let type_of_primitive = function
  | Int _
  | Char _ ->
      `Int
  | Bool _ -> `Bool

(** [type_check_primitive ctx p] is [Ok expr] where [expr] is a
    decorated expression node for [p] *)
let type_check_primitive ~ctx ~pos p =
  let e = Decorated.Expr.Primitive p in
  let typ = type_of_primitive p in
  Node.Expr.make ~ctx ~typ ~pos e

(** [type_check_id ctx i] is [Ok expr] where [expr] is a decorated
    expression node for [i] if [i] is in [ctx], or [Error err] otherwise *)
let type_check_id ~ctx ~pos id =
  let%map typ = ctx |> Context.find_var ~id |> map_error ~pos in
  let e = Decorated.Expr.Id id in
  Node.Expr.make ~ctx ~typ:(typ :> expr) ~pos e

(** [type_check_expr ctx enode] is [Ok expr] where [expr] is [enode]
    decorated within context [ctx] or [Error type_error] where
    [type_error] describes a semantic error of [enode] *)
let rec type_check_expr ~(ctx : Type.context) (enode : Expr.node) :
    Decorated.expr_result =
  let pos = PosNode.position enode in
  match Expr.Node.get enode with
  | Primitive p -> Ok (type_check_primitive ~ctx ~pos p)
  | Id id -> type_check_id ~ctx ~pos id
  | Array arr -> type_check_array ~ctx ~pos arr
  | String s -> Ok (type_check_string ~ctx ~pos s)
  | Bop (op, e1, e2) -> type_check_bop ~ctx ~pos op e1 e2
  | Uop (op, e) -> type_check_uop ~ctx ~pos op e
  | FnCall (id, es) -> type_check_fn_call ~ctx ~pos id es
  | Length node -> type_check_length ~ctx ~pos node
  | Index (e1, e2) -> type_check_index ~ctx ~pos e1 e2

and check_equality typ ctx acc elt =
  let%bind dec = type_check_expr ~ctx elt in
  let pos = PosNode.position elt in
  let e_typ = Node.Expr.typ dec in
  let e = Node.Expr.make ~ctx ~typ:e_typ ~pos dec in
  map_error ~pos (assert_eq_tau typ e_typ) >>| fun () -> e :: acc

and type_check_array ~ctx ~pos arr =
  if Array.is_empty arr then failwith "unimplemented"
  else
    let%bind dec = type_check_expr ~ctx (Array.get arr 0) in
    let typ = Node.Expr.typ dec in
    let%bind lst =
      Array.fold_result arr ~init:[] ~f:(check_equality typ ctx)
    in
    let dec_array = lst |> List.rev |> Array.of_list in
    let e = Decorated.Expr.Array dec_array in
    Ok (Node.Expr.make ~ctx ~typ ~pos e)

and type_check_string ~ctx ~pos str =
  let e = Decorated.Expr.String str in
  Node.Expr.make ~ctx ~typ:(`Array `Int) ~pos e

and type_check_bop ~ctx ~pos op e1 e2 =
  let%bind dec1 = type_check_expr ~ctx e1 in
  let%bind dec2 = type_check_expr ~ctx e2 in
  let e = Decorated.Expr.Bop (op, dec1, dec2) in
  match (op, Node.Expr.typ dec1, Node.Expr.typ dec2) with
  | (Plus | Minus | Mult | HighMult | Div | Mod), `Int, `Int ->
      Ok (Node.Expr.make ~ctx ~typ:`Int ~pos e)
  | (Lt | Leq | Gt | Geq), `Int, `Int
  | (And | Or), `Bool, `Bool ->
      Ok (Node.Expr.make ~ctx ~typ:`Bool ~pos e)
  | (Eq | Neq), t1, t2 ->
      map_error ~pos (assert_eq_tau t1 t2) >>| fun () ->
      Node.Expr.make ~ctx ~typ:`Bool ~pos e
  | _ -> failwith "unimplemented"

and type_check_uop ~ctx ~pos op e =
  let%bind dec = type_check_expr ctx e in
  let e = Decorated.Expr.Uop (op, dec) in
  match (op, Node.Expr.typ dec) with
  | IntNeg, `Int -> Ok (Node.Expr.make ~ctx ~typ:`Int ~pos e)
  | LogicalNeg, `Bool -> Ok (Node.Expr.make ~ctx ~typ:`Bool ~pos e)
  | _ -> map_error ~pos (Error OpMismatch)
(* TODO add op + type to opmismatch *)

and type_check_fn_call ~ctx ~pos id es =
  let%map t1, t2 = ctx |> Context.find_fun ~id |> map_error ~pos in
  failwith "unimplemented"

and type_check_length ~ctx ~pos node =
  let%bind dec = type_check_expr ctx node in
  let typ = Node.Expr.typ dec in
  let e = Decorated.Expr.Length dec in
  map_error ~pos (assert_array typ) >>| fun () ->
  Node.Expr.make ~ctx ~typ ~pos e

and type_of_array ~ctx ~pos arr = failwith "unimplemented"

and type_check_index ~ctx ~pos e1 e2 =
  let%bind dec1 = type_check_expr ctx e1 in
  let%bind dec2 = type_check_expr ctx e2 in
  let e = Decorated.Expr.Index (dec1, dec2) in
  match (Node.Expr.typ dec1, Node.Expr.typ dec2) with
  | `Array arr, `Int ->
      Ok (Node.Expr.make ~ctx ~typ:(type_of_array ~ctx ~pos arr) ~pos e)
  | _ ->
      map_error ~pos
        (Error (Mismatch (Node.Expr.typ dec1, Node.Expr.typ dec2)))

(** [bool_or_error ctx e] is [Ok e] if [e] is [Ok e] and [e] has bool
    type in context [ctx] and [Error Mismatch] otherwise *)
let bool_or_error ~ctx expr =
  let%bind e = type_check_expr ~ctx expr in
  assert_bool e >>| fun () -> e

(** [bool_or_error_stmt ctx e] is [Ok e] if [e] is [Ok e] and [e] has
    bool type in function context [ctx] and [Error Mismatch] otherwise *)
let bool_or_error_stmt ~ctx =
  bool_or_error ~ctx:(Context.Fn.context ctx)

let type_check_var_decl ~ctx ~pos id typ =
  let%map ctx = ctx |> Context.Fn.add_var ~id ~typ |> map_error ~pos in
  let s = Decorated.Stmt.VarDecl (id, typ) in
  Node.Stmt.make_unit s ~ctx ~pos

let type_check_array_decl ~ctx ~pos id typ es = failwith "unimplemented"

let type_check_assign ~ctx ~pos id e =
  let%bind lhs = ctx |> Context.Fn.find_var ~id |> map_error ~pos in
  let%bind e = type_check_expr (Context.Fn.context ctx) e in
  let%map () = assert_eq ~expect:lhs e in
  let s = Decorated.Stmt.Assign (id, e) in
  Node.Stmt.make_unit s ~ctx ~pos

let type_check_expr_stmt ~ctx:fn_ctx ~pos id es =
  let ctx = Context.Fn.context fn_ctx in
  let%map call = type_check_fn_call ~ctx ~pos id es in
  let s = Decorated.Stmt.ExprStmt (id, es) in
  Node.Stmt.make_unit s ~ctx:fn_ctx ~pos

let type_check_var_init ~ctx ~pos id typ e = failwith "unimplemented"

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
  | VarDecl (id, typ) -> type_check_var_decl ~ctx ~pos id typ
  | ArrayDecl (id, typ, es) -> type_check_array_decl ~ctx ~pos id typ es
  | Assign (id, e) -> type_check_assign ~ctx ~pos id e
  | ArrAssign (e1, e2, e3) -> failwith "unimplemented"
  | ExprStmt (id, es) -> type_check_expr_stmt ~ctx ~pos id es
  | VarInit (id, typ, e) -> failwith "unimplemented"
  | MultiAssign (ds, id, es) -> failwith "unimplemented"
  | PrCall (id, es) -> failwith "unimplemented"
  | Return es -> failwith "unimplemented"
  | Block stmts -> failwith "unimplemented"

and type_check_cond ~ctx e s =
  let%bind e = bool_or_error_stmt ~ctx e in
  let%map s = type_check_stmt ~ctx s in
  (e, s)

and make_cond ~f ~ctx ~pos e s =
  let%map e, s = type_check_cond ctx e s in
  Node.Stmt.make_unit ~ctx ~pos (f e s)

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

and type_check_prcall ~ctx ~pos id es =
  let%map t1, t2 = ctx |> Context.find_fun ~id |> map_error ~pos in
  match (t1, t2) with
  | `Unit, `Unit -> Node.Stmt.make s ~ctx ~typ:`Unit ~pos
  | _ -> failwith "unimplemented"
