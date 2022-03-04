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

(** [type_check_exprs ~ctx ~pos ~types es] is [Ok es'] where [es'] is
    [es] decorated if each of [es] typechecks and matches with its
    corresponding type in [types], [Error CountMismatch] if the length
    of the [types] and [es] differ, and [Error err] if [err] is
    propogated during the type-checking of any of [es] prior to a
    [CountMismatch] *)
and type_check_exprs ~ctx ~pos ~types es =
  match (types, es) with
  | typ :: types, e :: es ->
      let%bind e = type_check_expr ~ctx e in
      let%bind () = Node.Expr.assert_eq_sub ~expect:typ e in
      type_check_exprs ~ctx ~pos ~types es >>| List.cons e
  | [], [] -> Ok []
  | _ -> Error (Positioned.count_mismatch pos)

and type_check_array ~ctx ~pos arr =
  if Array.is_empty arr then failwith "any array"
  else
    let%bind dec = type_check_expr ~ctx (Array.get arr 0) in
    let typ = Node.Expr.typ dec in
    let%bind lst =
      Array.fold_result arr ~init:[] ~f:(fold_array ctx typ)
    in
    let dec_array = lst |> List.rev |> Array.of_list in
    let e = Decorated.Expr.Array dec_array in
    Ok (Node.Expr.make ~ctx ~typ ~pos e)

and fold_array ctx typ acc elt =
  let%bind dec = type_check_expr ~ctx elt in
  let pos = PosNode.position elt in
  let e_typ = Node.Expr.typ dec in
  map_error ~pos (assert_eq_tau typ e_typ) >>| fun () -> dec :: acc

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
  | _ -> Error (Positioned.make ~pos OpMismatch)

and type_check_uop ~ctx ~pos op e =
  let%bind dec = type_check_expr ctx e in
  let e = Decorated.Expr.Uop (op, dec) in
  match (op, Node.Expr.typ dec) with
  | IntNeg, `Int -> Ok (Node.Expr.make ~ctx ~typ:`Int ~pos e)
  | LogicalNeg, `Bool -> Ok (Node.Expr.make ~ctx ~typ:`Bool ~pos e)
  | _ -> Error (Positioned.make ~pos OpMismatch)
(* TODO add op + type to opmismatch *)

and type_check_call ~ctx ~pos id es =
  let%bind t1, t2 = ctx |> Context.find_fun ~id |> map_error ~pos in
  let types = tau_list_of_term t1 in
  let%map es = type_check_exprs ~ctx ~pos ~types es in
  (es, t2)

and type_check_fn_call ~ctx ~pos id es =
  let%map es, t2 = type_check_call ~ctx ~pos id es in
  let e = Decorated.Expr.FnCall (id, es) in
  Node.Expr.make ~ctx ~typ:(expr_of_term t2) ~pos e

and type_check_length ~ctx ~pos node =
  let%bind dec = type_check_expr ctx node in
  let typ = Node.Expr.typ dec in
  let e = Decorated.Expr.Length dec in
  map_error ~pos (assert_array typ) >>| fun () ->
  Node.Expr.make ~ctx ~typ ~pos e

and type_check_index ~ctx ~pos e1 e2 =
  let%bind dec1 = type_check_expr ctx e1 in
  let%bind dec2 = type_check_expr ctx e2 in
  let e = Decorated.Expr.Index (dec1, dec2) in
  let dec1_typ = Node.Expr.typ dec1 in
  let dec2_typ = Node.Expr.typ dec2 in
  match (dec1_typ, dec2_typ) with
  | `Array t, `Int -> Ok (Node.Expr.make ~ctx ~typ:(t :> expr) ~pos e)
  | _ -> Error (Positioned.make ~pos (Mismatch (dec1_typ, dec2_typ)))

let type_check_expr_fn_ctx ~ctx =
  type_check_expr ~ctx:(Context.Fn.context ctx)

(** [bool_or_error_stmt ctx e] is [Ok e] if [e] is [Ok e] and [e] has
    bool type in function context [ctx] and [Error Mismatch] otherwise *)
let bool_or_error ~ctx e =
  let%bind e = type_check_expr_fn_ctx ~ctx e in
  Node.Expr.assert_bool e >>| fun () -> e

let type_check_var_decl ~ctx ~pos id typ =
  let%map ctx = ctx |> Context.Fn.add_var ~id ~typ |> map_error ~pos in
  let s = Decorated.Stmt.VarDecl (id, typ) in
  Node.Stmt.make_unit s ~ctx ~pos

(** [type_check_empty es] is [Ok \[\]] if each of [es] is [None], or
    [Error err] if any of [es] are [Some _]*)
let rec type_check_empty = function
  | [] -> Ok []
  | None :: es -> type_check_empty es
  | Some e :: _ ->
      Error (e |> PosNode.position |> Positioned.illegal_arr_decl)

(** [type_check_sizes ~ctx es] is [Ok \[e1; ...; em\]] if [es] is
    [Some e1; ... Some en; None ...] or [Error err] if [Some _] follows
    [None] in [es] *)
let rec type_check_sizes ~ctx = function
  | [] -> Ok []
  | Some e :: es ->
      let%bind e = type_check_expr ~ctx e in
      let%bind () = Node.Expr.assert_int e in
      type_check_sizes ~ctx es >>| List.cons e
  | None :: es -> type_check_empty es

let type_check_array_decl ~ctx ~pos id typ es =
  let%bind ctx = ctx |> Context.Fn.add_var ~id ~typ |> map_error ~pos in
  let%map es = type_check_sizes ~ctx:(Context.Fn.context ctx) es in
  let es = List.map ~f:Option.some es in
  let s = Decorated.Stmt.ArrayDecl (id, typ, es) in
  Node.Stmt.make_unit ~ctx ~pos s

let type_check_assign ~ctx ~pos id e =
  let%bind typ = ctx |> Context.Fn.find_var ~id |> map_error ~pos in
  let%bind dec = type_check_expr_fn_ctx ~ctx e in
  let%map () = Node.Expr.assert_eq_sub ~expect:typ dec in
  let s = Decorated.Stmt.Assign (id, dec) in
  Node.Stmt.make_unit ~ctx ~pos s

let type_check_expr_stmt ~ctx:fn_ctx ~pos id es =
  let ctx = Context.Fn.context fn_ctx in
  let%map es, _ = type_check_call ~ctx ~pos id es in
  let s = Decorated.Stmt.ExprStmt (id, es) in
  Node.Stmt.make_unit ~ctx:fn_ctx ~pos s

let type_check_var_init ~ctx ~pos id typ e =
  let%bind ctx = ctx |> Context.Fn.add_var ~id ~typ |> map_error ~pos in
  let%bind dec = type_check_expr_fn_ctx ~ctx e in
  let%map () = Node.Expr.assert_eq_sub ~expect:typ dec in
  let s = Decorated.Stmt.VarInit (id, typ, dec) in
  Node.Stmt.make_unit ~ctx ~pos s

let type_check_arr_assign ~ctx ~pos e1 e2 e3 =
  let%bind dec1 = type_check_expr_fn_ctx ~ctx e1 in
  let%bind dec2 = type_check_expr_fn_ctx ~ctx e2 in
  let%bind dec3 = type_check_expr_fn_ctx ~ctx e3 in
  let pos1 = Node.Expr.position dec1 in
  let pos2 = Node.Expr.position dec2 in
  let s = Decorated.Stmt.ArrAssign (dec1, dec2, dec3) in
  match
    (Node.Expr.typ dec1, Node.Expr.typ dec2, Node.Expr.typ dec3)
  with
  | `Array t1, `Int, t2 ->
      map_error ~pos (assert_eq_tau t1 t2) >>| fun () ->
      Node.Stmt.make_unit s ~ctx ~pos
  | _, `Int, _ -> Error (Positioned.make ~pos:pos1 ExpectedArray)
  | _, t, _ -> Error (Positioned.make ~pos:pos2 (Mismatch (`Int, t)))

let rec check_decls ~ctx ~pos ds ts =
  match (ds, ts) with
  | [], [] -> Ok ctx
  | decl :: decls, typ :: typs -> begin
      match (decl, typ) with
      | Some (id, tau), t ->
          let%bind ctx =
            ctx |> Context.Fn.add_var ~id ~typ:tau |> map_error ~pos
          in
          check_decls ~ctx ~pos decls typs
      | None, _ -> check_decls ~ctx ~pos decls typs
    end
  | _ -> Error (Positioned.count_mismatch pos)

let type_check_multi_assign ~ctx:fn_ctx ~pos ds id es =
  let ctx = Context.Fn.context fn_ctx in
  let%bind es, t2 = type_check_call ~ctx ~pos id es in
  let s = Decorated.Stmt.MultiAssign (ds, id, es) in
  let%map fn_ctx =
    check_decls ~ctx:fn_ctx ~pos ds (tau_list_of_term t2)
  in
  Node.Stmt.make_unit ~ctx:fn_ctx ~pos s

let type_check_return ~ctx:fn_ctx ~pos es =
  let rho = Context.Fn.ret fn_ctx in
  let ctx = Context.Fn.context fn_ctx in
  let%bind s_lst =
    type_check_exprs ~ctx ~pos ~types:(tau_list_of_term rho) es
  in
  let s = Decorated.Stmt.Return s_lst in
  Ok (Node.Stmt.make_void s ~ctx:fn_ctx ~pos)

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
  | ArrAssign (e1, e2, e3) -> type_check_arr_assign ~ctx ~pos e1 e2 e3
  | ExprStmt (id, es) -> type_check_expr_stmt ~ctx ~pos id es
  | VarInit (id, typ, e) -> type_check_var_init ~ctx ~pos id typ e
  | MultiAssign (ds, id, es) ->
      type_check_multi_assign ~ctx ~pos ds id es
  | PrCall (id, es) -> type_check_pr_call ~ctx ~pos id es
  | Return es -> type_check_return ~ctx ~pos es
  | Block stmts -> type_check_block ~ctx ~pos stmts

and type_check_cond ~ctx e s =
  let%bind e = bool_or_error ~ctx e in
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
  Node.Stmt.make ~ctx ~pos ~typ s

and type_check_while ~ctx ~pos e s =
  make_cond ~f:(fun e s -> Decorated.Stmt.While (e, s)) ~ctx ~pos e s

and type_check_pr_call ~ctx:fn_ctx ~pos id es =
  let ctx = Context.Fn.context fn_ctx in
  let%bind es, t2 = type_check_call ~ctx ~pos id es in
  let s = Decorated.Stmt.PrCall (id, es) in
  map_error ~pos (assert_unit t2) >>| fun () ->
  Node.Stmt.make_unit ~ctx:fn_ctx ~pos s

and type_check_block ~ctx ~pos stmts =
  let%map stmts, typ = type_check_stmts ~ctx [] stmts in
  let s = Decorated.Stmt.Block stmts in
  Node.Stmt.make ~ctx ~pos ~typ s

and type_check_stmts ~ctx acc = function
  | [] -> Ok ([], `Unit)
  | s :: stmts ->
      let%bind s = type_check_stmt ~ctx s in
      let acc = s :: acc in
      if List.is_empty stmts then Ok (List.rev acc, Node.Stmt.typ s)
      else
        let%bind () = Node.Stmt.assert_unit s in
        type_check_stmts ~ctx:(Node.Stmt.context s) acc stmts
