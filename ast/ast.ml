open Core
open Result.Monad_infix
open Result.Let_syntax
open Util.Result
module Node = Node.Position
include Factory.Make (Node) (Node) (Node)
include Abstract
module PosNode = Node
open Expr
open Stmt
open Toplevel
open Primitive
open Type

(** [map_error ~pos result] decorates the error variant of [result] with
    the position [pos] at which it occurs *)
let map_error ~pos = Result.map_error ~f:(Decorated.Error.make ~pos)

(** [r >>? pos] is [map_error ~pos r] *)
let ( >>? ) r pos = map_error ~pos r

(** fold2 ~pos ~f ~init:a [b1; ...; bn] [c1; ...; cn] is
    [f (... (f (f a b1 c1) b2 c2) ...) bn cn], short circuiting on
    return of [Error _]. If the lengths of [l1] and [l2] are not equal,
    [Error CountMismatch] is returned. *)
let fold2_result ~pos =
  let unequal_lengths = Positioned.count_mismatch pos in
  Util.List.fold2_result ~unequal_lengths

(** [type_of_primitive p] is [`Int] if [p] is [Int] or [Char] and
    [`Bool] if [p] is [Bool] *)
let type_of_primitive = function
  | Int _ | Char _ -> `Int
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
  let%map typ = Context.find_var ~id ctx in
  let e = Decorated.Expr.Id id in
  Node.Expr.make ~ctx ~typ:(typ :> expr) ~pos e

(** [type_check_expr ctx enode] is [Ok expr] where [expr] is [enode]
    decorated within context [ctx] or [Error type_error] where
    [type_error] describes a semantic error of [enode] *)
let rec type_check_expr ~ctx enode =
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
  let f acc typ e =
    let%bind e = type_check_expr ~ctx e in
    let%map () = Node.Expr.assert_eq_sub ~expect:typ e in
    e :: acc
  in
  fold2_result ~f ~pos ~init:[] types es >>| List.rev

and type_check_array ~ctx ~pos arr =
  if Array.is_empty arr then
    let empty_array = Decorated.Expr.Array [||] in
    let typ = `Array `Poly in
    Ok (Node.Expr.make ~ctx ~typ ~pos empty_array)
  else
    let%bind dec = type_check_expr ~ctx (Array.get arr 0) in
    let%bind lst =
      Array.fold_result arr ~init:[] ~f:(fold_array ctx dec)
    in
    let dec_array = lst |> List.rev |> Array.of_list in
    let e = Decorated.Expr.Array dec_array in
    let%map typ = tau_of_expr_res (Node.Expr.typ dec) pos in
    Node.Expr.make ~ctx ~typ:(`Array typ) ~pos e

and fold_array ctx fst acc elt =
  let%bind dec = type_check_expr ~ctx elt in
  Node.Expr.assert_eq_tau fst dec >>| fun () -> dec :: acc

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
  | Plus, `Array t1, `Array t2 ->
      if Tau.equal t1 t2 then
        Ok (Node.Expr.make ~ctx ~typ:(`Array t1) ~pos e)
      else Error (Positioned.make ~pos OpMismatch)
  | (Lt | Leq | Gt | Geq), `Int, `Int | (And | Or), `Bool, `Bool ->
      Ok (Node.Expr.make ~ctx ~typ:`Bool ~pos e)
  | (Eq | Neq), _, _ ->
      Node.Expr.assert_eq_tau dec1 dec2 >>| fun () ->
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
  let%bind t1, t2 = Context.find_fn ~id ctx in
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
  assert_array typ >>? pos >>| fun () -> Node.Expr.make ~ctx ~typ ~pos e

and type_check_index ~ctx ~pos e1 e2 =
  let%bind dec1 = type_check_expr ctx e1 in
  let%bind dec2 = type_check_expr ctx e2 in
  let e = Decorated.Expr.Index (dec1, dec2) in
  let dec1_typ = Node.Expr.typ dec1 in
  let dec2_typ = Node.Expr.typ dec2 in
  match (dec1_typ, dec2_typ) with
  | `Array t, `Int -> Ok (Node.Expr.make ~ctx ~typ:(t :> expr) ~pos e)
  | _ -> Error (Positioned.mismatch pos ~expect:dec1_typ dec2_typ)

(** [bool_or_error_stmt ctx e] is [Ok e] if [e] is [Ok e] and [e] has
    bool type in function context [ctx] and [Error Mismatch] otherwise *)
let bool_or_error ~ctx e =
  let%bind e = type_check_expr ~ctx e in
  Node.Expr.assert_bool e >>| fun () -> e

let type_check_var_decl ~ctx ~pos id typ =
  let%map ctx = Context.add_var ~id ~typ ctx in
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
  | None :: es -> type_check_empty es
  | Some e :: es ->
      let%bind e = type_check_expr ~ctx e in
      let%bind () = Node.Expr.assert_int e in
      type_check_sizes ~ctx es >>| List.cons e

let type_check_array_decl ~ctx ~pos id typ es =
  let%bind ctx = Context.add_var ~id ~typ ctx in
  let%map es = type_check_sizes ~ctx es in
  let es = List.map ~f:Option.some es in
  let s = Decorated.Stmt.ArrayDecl (id, typ, es) in
  Node.Stmt.make_unit ~ctx ~pos s

let type_check_assign ~ctx ~pos id e =
  let%bind typ = Context.find_var ~id ctx in
  let%bind dec = type_check_expr ~ctx e in
  let%map () = Node.Expr.assert_eq_sub ~expect:typ dec in
  let s = Decorated.Stmt.Assign (id, dec) in
  Node.Stmt.make_unit ~ctx ~pos s

let type_check_expr_stmt ~ctx ~pos id es =
  let%map es, _ = type_check_call ~ctx ~pos id es in
  let s = Decorated.Stmt.ExprStmt (id, es) in
  Node.Stmt.make_unit ~ctx ~pos s

let type_check_var_init ~ctx ~pos id typ e =
  let%bind ctx = Context.add_var ~id ~typ ctx in
  let%bind dec = type_check_expr ~ctx e in
  let%map () = Node.Expr.assert_eq_sub ~expect:typ dec in
  let s = Decorated.Stmt.VarInit (id, typ, dec) in
  Node.Stmt.make_unit ~ctx ~pos s

let type_check_arr_assign ~ctx ~pos e1 e2 e3 =
  let%bind dec1 = type_check_expr ~ctx e1 in
  let%bind dec2 = type_check_expr ~ctx e2 in
  let%bind dec3 = type_check_expr ~ctx e3 in
  let pos1 = Node.Expr.position dec1 in
  let pos2 = Node.Expr.position dec2 in
  let s = Decorated.Stmt.ArrAssign (dec1, dec2, dec3) in
  match
    (Node.Expr.typ dec1, Node.Expr.typ dec2, Node.Expr.typ dec3)
  with
  | `Array t, `Int, _ ->
      Node.Expr.assert_eq_sub ~expect:t dec2 >>| fun () ->
      Node.Stmt.make_unit s ~ctx ~pos
  | _, `Int, _ -> Error (Positioned.expected_array pos1)
  | _, t, _ -> Error (Positioned.mismatch pos2 ~expect:`Int t)

let check_decls ~ctx ~pos ds ts =
  let f ctx d typ =
    match d with
    | Some (id, tau) ->
        let error () = Positioned.mismatch pos ~expect:typ tau in
        let%bind () = ok_if_true_lazy (Tau.equal typ tau) ~error in
        Context.add_var ~id ~typ ctx
    | None -> Ok ctx
  in
  fold2_result ~pos ~f ~init:ctx ds ts

let type_check_multi_assign ~ctx ~pos ds id es =
  let%bind es, t2 = type_check_call ~ctx ~pos id es in
  let s = Decorated.Stmt.MultiAssign (ds, id, es) in
  let ts = tau_list_of_term t2 in
  let%map ctx = check_decls ~ctx ~pos ds ts in
  Node.Stmt.make_unit ~ctx ~pos s

let type_check_return ~ctx ~pos es =
  let rho = Context.ret ctx in
  let types = tau_list_of_term rho in
  let%map s_lst = type_check_exprs ~ctx ~pos ~types es in
  let s = Decorated.Stmt.Return s_lst in
  Node.Stmt.make_void s ~ctx ~pos

(** [type_check_stmt ctx snode] is [Ok stmt] where [stmt] is [snode]
    decorated within function context [ctx] or [Error type_error] where
    [type_error] describes a semantic error of [snode] *)
let rec type_check_stmt ~ctx snode =
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
  let typ = Node.Stmt.lub s1 s2 in
  let s = Decorated.Stmt.IfElse (e, s1, s2) in
  Node.Stmt.make ~ctx ~pos ~typ s

and type_check_while ~ctx ~pos e s =
  make_cond ~f:(fun e s -> Decorated.Stmt.While (e, s)) ~ctx ~pos e s

and type_check_pr_call ~ctx ~pos id es =
  let%bind es, t2 = type_check_call ~ctx ~pos id es in
  let s = Decorated.Stmt.PrCall (id, es) in
  assert_unit t2 >>? pos >>| fun () -> Node.Stmt.make_unit ~ctx ~pos s

and type_check_block ~ctx ~pos stmts =
  let%map stmts, typ = type_check_stmts ~ctx stmts in
  let s = Decorated.Stmt.Block stmts in
  Node.Stmt.make ~ctx ~pos ~typ s

and type_check_stmts ~ctx = type_check_stmts_acc ~ctx []

and type_check_stmts_acc ~ctx acc = function
  | [] -> Ok ([], `Unit)
  | s :: stmts ->
      let%bind s = type_check_stmt ~ctx s in
      let acc = s :: acc in
      if List.is_empty stmts then Ok (List.rev acc, Node.Stmt.typ s)
      else
        let%bind () = Node.Stmt.assert_unit s in
        type_check_stmts_acc ~ctx:(Node.Stmt.context s) acc stmts

let type_check_file ~ctx file = failwith "unimplemented"

(** [fold_decls ~ctx ds] is [Ok ctx'] where [ctx'] is [ctx] extended
    with every binding in [ds] if [ds] and [ctx] are disjoint, or
    [Error] otherwise *)
let fold_decls ~ctx ~pos =
  let f ctx (id, typ) = Context.add_var ~id ~typ ctx in
  List.fold_result ~init:ctx ~f

let signature_contexts ~ctx ~pos { id; params; types } =
  let arg = term_of_tau_list (List.map ~f:snd params) in
  let ret = term_of_tau_list types in
  let%bind ctx = Context.add_fn ~id ~arg ~ret ctx in
  let%map fn_ctx = fold_decls ~ctx ~pos params in
  (ctx, Context.with_ret ~ret fn_ctx)

let type_check_function ~ctx ~pos signature block =
  let%bind ctx, fn_ctx = signature_contexts ~ctx ~pos signature in
  let%map block, typ = type_check_stmts ~ctx:fn_ctx block in
  let fn_defn = Decorated.Toplevel.FnDefn (signature, block) in
  Node.Toplevel.make ~ctx ~pos fn_defn

let check_use ~ctx use =
  let intf = PosNode.get use in
  let pos = PosNode.position use in
  Ok (Node.Toplevel.make ~ctx ~pos intf)

(** [fold_context ~f ~ctx nodes] folds [f] over each node of [nodes],
    returning a pair [(ctx', nodes')] where [ctx'] is the decorated
    context and [nodes'] are the decorated nodes *)
let fold_context ~f ~ctx nodes =
  let fold (ctx, acc) node =
    let%map node = f ~ctx node in
    (Node.Toplevel.context node, node :: acc)
  in
  let init = (ctx, []) in
  let%map ctx, nodes = List.fold_result ~init ~f:fold nodes in
  (ctx, List.rev nodes)

let check_uses = fold_context ~f:check_use

let check_global_decl ~ctx ~pos id typ =
  let%map ctx = Context.add_var ~id ~typ ctx in
  let globdecl = Decorated.Toplevel.GlobalDecl (id, typ) in
  Node.Toplevel.make ~ctx ~pos globdecl

let check_global_init ~ctx ~pos id tau primitive =
  let%bind ctx = Context.add_var ~id ~typ:tau ctx in
  let p_type = type_of_primitive primitive in
  let tau_type = (tau :> expr) in
  if Node.Expr.typ_equal tau_type p_type then
    Ok
      (Node.Toplevel.make ~ctx ~pos
         (Decorated.Toplevel.GlobalInit (id, tau, primitive)))
  else Error (Positioned.mismatch pos ~expect:tau_type p_type)

let check_defn ~ctx node =
  let pos = PosNode.position node in
  match PosNode.get node with
  | FnDefn (signature, block) ->
      type_check_function ~ctx ~pos signature block
  | GlobalDecl (id, typ) -> check_global_decl ~ctx ~pos id typ
  | GlobalInit (id, tau, primitive) ->
      check_global_init ~ctx ~pos id tau primitive

let check_defs ~ctx defs = fold_context ~f:check_defn ~ctx defs >>| snd

let type_check_signature ~ctx signode =
  let signature = PosNode.get signode in
  let pos = PosNode.position signode in
  let%map ctx, _ = signature_contexts ~pos ~ctx signature in
  Node.Toplevel.make ~ctx ~pos signature

let type_check_source ~ctx { uses; definitions } =
  let%bind ctx, uses = check_uses ~ctx uses in
  let%map definitions = check_defs ~ctx definitions in
  let source : Decorated.Toplevel.source = { uses; definitions } in
  source

let rec type_check_interface ~ctx sigs =
  fold_context ~f:type_check_signature ~ctx sigs >>| snd

(* TODO toplevel position *)
let type_check prog =
  let ctx = Context.empty in
  match prog with
  | Source source ->
      let%map source = type_check_source ~ctx source in
      Decorated.Source source
  | Interface interface ->
      let%map interface = type_check_interface ~ctx interface in
      Decorated.Interface interface

module Decorated = Decorated
