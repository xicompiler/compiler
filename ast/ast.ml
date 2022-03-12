open Core
open Result.Monad_infix
open Result.Let_syntax
open Util.Result
module PosNode = Node.Position
module DecNode = Context.Node.Decorated
include Factory.Make (PosNode) (PosNode) (PosNode)
include Abstract
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
  DecNode.Expr.make ~ctx ~typ ~pos e

(** [type_check_id ctx i] is [Ok expr] where [expr] is a decorated
    expression node for [i] if [i] is in [ctx], or [Error err] otherwise *)
let type_check_id ~ctx ~pos id =
  let%map typ = Context.find_var ~id ctx in
  let e = Decorated.Expr.Id id in
  DecNode.Expr.make ~ctx ~typ:(typ :> expr) ~pos e

let rec type_check_expr ~ctx enode =
  let pos = PosNode.position enode in
  match PosNode.get enode with
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
    let%map () = DecNode.Expr.assert_eq_sub ~expect:typ e in
    e :: acc
  in
  fold2_result ~f ~pos ~init:[] types es >>| List.rev

(** [type_check_array ~ctx ~pos arr] is [Ok arr'] where [arr'] is [arr]
    decorated, or [Error type_error] where [type_error] describes the
    type error otherwise *)
and type_check_array ~ctx ~pos arr =
  if Array.is_empty arr then
    let empty_array = Decorated.Expr.Array [||] in
    let typ = `Array `Poly in
    Ok (DecNode.Expr.make ~ctx ~typ ~pos empty_array)
  else
    let%bind dec = type_check_expr ~ctx (Array.get arr 0) in
    let%bind lst =
      Array.fold_result arr ~init:[] ~f:(fold_array ctx dec)
    in
    let dec_array = lst |> List.rev |> Array.of_list in
    let e = Decorated.Expr.Array dec_array in
    let%map typ = tau_of_expr_res (DecNode.Expr.typ dec) pos in
    DecNode.Expr.make ~ctx ~typ:(`Array typ) ~pos e

(** [fold_array ctx fst acc elt] is [Ok lst] where [lst] is [elt]
    decorated and appended to [acc], ensuring that its type is equal to
    that of [fst], or [Error type_error] where [type_error] describes
    the type error otherwise *)
and fold_array ctx fst acc elt =
  let%bind dec = type_check_expr ~ctx elt in
  DecNode.Expr.assert_eq_tau fst dec >>| fun () -> dec :: acc

(** [type_check_string ~ctx ~pos str] is [Ok str'] where [str'] is [str]
    decorated, or [Error type_error] where [type_error] describes the
    type error otherwise *)
and type_check_string ~ctx ~pos str =
  let e = Decorated.Expr.String str in
  DecNode.Expr.make ~ctx ~typ:(`Array `Int) ~pos e

(** [type_check_bop ~ctx ~pos op e1 e2] is [Ok bop] where [bop] is Bop
    ([op], [e1], [e2]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
and type_check_bop ~ctx ~pos op e1 e2 =
  let%bind dec1 = type_check_expr ~ctx e1 in
  let%bind dec2 = type_check_expr ~ctx e2 in
  let e = Decorated.Expr.Bop (op, dec1, dec2) in
  match (op, DecNode.Expr.typ dec1, DecNode.Expr.typ dec2) with
  | (Plus | Minus | Mult | HighMult | Div | Mod), `Int, `Int ->
      Ok (DecNode.Expr.make ~ctx ~typ:`Int ~pos e)
  | Plus, `Array t1, `Array t2 ->
      if Tau.equal t1 t2 then
        Ok (DecNode.Expr.make ~ctx ~typ:(`Array t1) ~pos e)
      else Error (Positioned.make ~pos OpMismatch)
  | (Lt | Leq | Gt | Geq), `Int, `Int | (And | Or), `Bool, `Bool ->
      Ok (DecNode.Expr.make ~ctx ~typ:`Bool ~pos e)
  | (Eq | Neq), _, _ ->
      DecNode.Expr.assert_eq_tau dec1 dec2 >>| fun () ->
      DecNode.Expr.make ~ctx ~typ:`Bool ~pos e
  | _ -> Error (Positioned.make ~pos OpMismatch)

(** [type_check_uop ~ctx ~pos op e] is [Ok uop] where [uop] is Uop
    ([op], [e]) decorated, or [Error type_error] where [type_error]
    describes the type error otherwise *)
and type_check_uop ~ctx ~pos op e =
  let%bind dec = type_check_expr ctx e in
  let e = Decorated.Expr.Uop (op, dec) in
  match (op, DecNode.Expr.typ dec) with
  | IntNeg, `Int -> Ok (DecNode.Expr.make ~ctx ~typ:`Int ~pos e)
  | LogicalNeg, `Bool -> Ok (DecNode.Expr.make ~ctx ~typ:`Bool ~pos e)
  | _ -> Error (Positioned.make ~pos OpMismatch)

(** [type_check_call ~ctx ~pos id es] is [Ok (es', t)] where [es'] is
    the nodes in [es] decorated and [t] is the return type of function
    [id], or [Error type_error] where [type_error] describes the type
    error otherwise *)
and type_check_call ~ctx ~pos id es =
  let%bind t1, t2 = Context.find_fn ~id ctx in
  let types = tau_list_of_term t1 in
  let%map es = type_check_exprs ~ctx ~pos ~types es in
  (es, t2)

(** [type_check_fn_call ~ctx ~pos id es] is [Ok fn] where [fn] is FnCall
    ([id], [es]) decorated, or [Error type_error] where [type_error]
    describes the type error otherwise *)
and type_check_fn_call ~ctx ~pos id es =
  let%map es, t2 = type_check_call ~ctx ~pos id es in
  let e = Decorated.Expr.FnCall (id, es) in
  DecNode.Expr.make ~ctx ~typ:(expr_of_term t2) ~pos e

(** [type_check_length ~ctx ~pos node] is [Ok ln] where [ln] is Length
    [node] decorated, or [Error type_error] where [type_error] describes
    the type error otherwise *)
and type_check_length ~ctx ~pos node =
  let%bind dec = type_check_expr ctx node in
  let typ = DecNode.Expr.typ dec in
  let e = Decorated.Expr.Length dec in
  assert_array typ >>? pos >>| fun () ->
  DecNode.Expr.make ~ctx ~typ:`Int ~pos e

(** [type_check_index ~ctx ~pos e1 e2] is [Ok idx] where [idx] is Index
    ([e1], [e2]) decorated, or [Error type_error] where [type_error]
    describes the type error otherwise *)
and type_check_index ~ctx ~pos e1 e2 =
  let%bind dec1 = type_check_expr ctx e1 in
  let%bind dec2 = type_check_expr ctx e2 in
  let e = Decorated.Expr.Index (dec1, dec2) in
  let dec1_typ = DecNode.Expr.typ dec1 in
  let dec2_typ = DecNode.Expr.typ dec2 in
  match (dec1_typ, dec2_typ) with
  | `Array t, `Int ->
      Ok (DecNode.Expr.make ~ctx ~typ:(t :> expr) ~pos e)
  | _ -> Error (Positioned.mismatch pos ~expect:dec1_typ dec2_typ)

(** [bool_or_error_stmt ctx e] is [Ok e] if [e] is [Ok e] and [e] has
    bool type in function context [ctx] and [Error Mismatch] otherwise *)
let bool_or_error ~ctx e =
  let%bind e = type_check_expr ~ctx e in
  DecNode.Expr.assert_bool e >>| fun () -> e

(** [type_check_var_decl ~ctx ~pos id typ] is [Ok vd] where [vd] is
    VarDecl ([id], [typ]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
let type_check_var_decl ~ctx ~pos id typ =
  let%map ctx = Context.add_var ~id ~typ ctx in
  let s = Decorated.Stmt.VarDecl (id, typ) in
  DecNode.Stmt.make_unit s ~ctx ~pos

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
      let%bind () = DecNode.Expr.assert_int e in
      type_check_sizes ~ctx es >>| List.cons e

(** [type_check_array_decl ~ctx ~pos id typ es] is [Ok ad] where [ad] is
    ArrayDecl ([id], [typ], [es]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
let type_check_array_decl ~ctx ~pos id typ es =
  let%bind ctx = Context.add_var ~id ~typ ctx in
  let%map es = type_check_sizes ~ctx es in
  let es = List.map ~f:Option.some es in
  let s = Decorated.Stmt.ArrayDecl (id, typ, es) in
  DecNode.Stmt.make_unit ~ctx ~pos s

(** [type_check_array_decl ~ctx ~pos id e] is [Ok assn] where [assn] is
    Assign ([id], [e]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
let type_check_assign ~ctx ~pos id e =
  let%bind typ = Context.find_var ~id ctx in
  let%bind dec = type_check_expr ~ctx e in
  let%map () = DecNode.Expr.assert_eq_sub ~expect:typ dec in
  let s = Decorated.Stmt.Assign (id, dec) in
  DecNode.Stmt.make_unit ~ctx ~pos s

(** [type_check_expr_stmt ~ctx ~pos id es] is [Ok es] where [es] is
    ExprStmt ([id], [es]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
let type_check_expr_stmt ~ctx ~pos id es =
  let%map es, _ = type_check_call ~ctx ~pos id es in
  let s = Decorated.Stmt.ExprStmt (id, es) in
  DecNode.Stmt.make_unit ~ctx ~pos s

(** [type_check_var_init ~ctx ~pos id typ e] is [Ok vi] where [vi] is
    VarInit ([id], [typ], [e]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
let type_check_var_init ~ctx ~pos id typ e =
  let%bind ctx = Context.add_var ~id ~typ ctx in
  let%bind dec = type_check_expr ~ctx e in
  let%map () = DecNode.Expr.assert_eq_sub ~expect:typ dec in
  let s = Decorated.Stmt.VarInit (id, typ, dec) in
  DecNode.Stmt.make_unit ~ctx ~pos s

(** [type_check_arr_assign ~ctx ~pos e1 e2 e3] is [Ok assn] where [assn]
    is ArrayAssign ([e1], [e2], [e3]) decorated, or [Error type_error]
    where [type_error] describes the type error otherwise *)
let type_check_arr_assign ~ctx ~pos e1 e2 e3 =
  let%bind dec1 = type_check_expr ~ctx e1 in
  let%bind dec2 = type_check_expr ~ctx e2 in
  let%bind dec3 = type_check_expr ~ctx e3 in
  let pos1 = DecNode.Expr.position dec1 in
  let pos2 = DecNode.Expr.position dec2 in
  let s = Decorated.Stmt.ArrAssign (dec1, dec2, dec3) in
  match
    (DecNode.Expr.typ dec1, DecNode.Expr.typ dec2, DecNode.Expr.typ dec3)
  with
  | `Array t, `Int, _ ->
      DecNode.Expr.assert_eq_sub ~expect:t dec3 >>| fun () ->
      DecNode.Stmt.make_unit s ~ctx ~pos
  | _, `Int, _ -> Error (Positioned.expected_array pos1)
  | _, t, _ -> Error (Positioned.mismatch pos2 ~expect:`Int t)

(** [check_decls ~ctx ~pos ds ts] is [Ok ctx'] where [ctx'] is [ctx]
    updated with variables [ds] and types [ts], or [Error type_error]
    where [type_error] describes the type error otherwise *)
let check_decls ~ctx ~pos ds ts =
  let f ctx d typ =
    match d with
    | Some (id, tau) ->
        let error () = Positioned.mismatch pos ~expect:typ tau in
        let%bind () = Lazy.ok_if_true (Tau.equal typ tau) ~error in
        Context.add_var ~id ~typ ctx
    | None -> Ok ctx
  in
  fold2_result ~pos ~f ~init:ctx ds ts

(** [type_check_multi_assign ~ctx ~pos ds id es] is [Ok mult] where
    [mult] is MultiAssign ([ds], [id], [es]) decorated, or
    [Error type_error] where [type_error] describes the type error
    otherwise *)
let type_check_multi_assign ~ctx ~pos ds id es =
  let%bind es, t2 = type_check_call ~ctx ~pos id es in
  let s = Decorated.Stmt.MultiAssign (ds, id, es) in
  let ts = tau_list_of_term t2 in
  let%map ctx = check_decls ~ctx ~pos ds ts in
  DecNode.Stmt.make_unit ~ctx ~pos s

(** [type_check_return ~ctx ~pos es] is [Ok ret] where [ret] is Return
    [es] decorated, or [Error type_error] where [type_error] describes
    the type error otherwise *)
let type_check_return ~ctx ~pos es =
  let rho = Context.ret ctx in
  let types = tau_list_of_term rho in
  let%map s_lst = type_check_exprs ~ctx ~pos ~types es in
  let s = Decorated.Stmt.Return s_lst in
  DecNode.Stmt.make_void s ~ctx ~pos

let rec type_check_stmt ~ctx snode =
  let pos = PosNode.position snode in
  match PosNode.get snode with
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

(** [type_check_cond ~ctx e s] is [Ok (e', s')] where [e'] is [Ok \[e\]]
    if [e] is a bool in [~ctx], and [s'] is the decorated node of [s].
    Otherwise, it is [Error type_error] where [type_error] describes the
    type error *)
and type_check_cond ~ctx e s =
  let%bind e = bool_or_error ~ctx e in
  let%map s = type_check_stmt ~ctx s in
  (e, s)

(** [make_cond ~f ~ctx ~pos e s] is [Ok u] where [u] is a decorated unit
    node that carries the result of applying [f] to [e] and [s], or
    [Error type_error] where [type_error] describes the type error
    otherwise *)
and make_cond ~f ~ctx ~pos e s =
  let%map e, s = type_check_cond ctx e s in
  DecNode.Stmt.make_unit ~ctx ~pos (f e s)

(** [type_check_if ~ctx ~pos e s] is [Ok if] where [if] is If ([e], [s])
    decorated, or [Error type_error] where [type_error] describes the
    type error otherwise *)
and type_check_if ~ctx ~pos e s =
  make_cond ~f:(fun e s -> Decorated.Stmt.If (e, s)) ~ctx ~pos e s

(** [type_check_if_else ~ctx ~pos e s1 s2] is [Ok ifel] where [ifel] is
    IfElse ([e], [s1], [s2]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
and type_check_if_else ~ctx ~pos e s1 s2 =
  let%bind e, s1 = type_check_cond ctx e s1 in
  let%map s2 = type_check_stmt ctx s2 in
  let typ = DecNode.Stmt.lub s1 s2 in
  let s = Decorated.Stmt.IfElse (e, s1, s2) in
  DecNode.Stmt.make ~ctx ~pos ~typ s

(** [type_check_while ~ctx ~pos e s] is [Ok w] where [w] is While ([e],
    [s]) decorated, or [Error type_error] where [type_error] describes
    the type error otherwise *)
and type_check_while ~ctx ~pos e s =
  make_cond ~f:(fun e s -> Decorated.Stmt.While (e, s)) ~ctx ~pos e s

(** [type_check_pr_call ~ctx ~pos id es] is [Ok pr] where [pr] is PrCall
    ([id], [es]) decorated, or [Error type_error] where [type_error]
    describes the type error otherwise *)
and type_check_pr_call ~ctx ~pos id es =
  let%bind es, t2 = type_check_call ~ctx ~pos id es in
  let s = Decorated.Stmt.PrCall (id, es) in
  assert_unit t2 >>? pos >>| fun () ->
  DecNode.Stmt.make_unit ~ctx ~pos s

(** [type_check_block ~ctx ~pos stmts] is [Ok block] where [block] is
    Block [stmts] decorated, or [Error type_error] where [type_error]
    describes the type error otherwise *)
and type_check_block ~ctx ~pos stmts =
  let%map stmts, typ = type_check_stmts ~ctx stmts in
  let s = Decorated.Stmt.Block stmts in
  DecNode.Stmt.make ~ctx ~pos ~typ s

(** [type_check_stmts ~ctx block] is [Ok (lst, typ)] where [lst] is a
    block of the decorated nodes of [block] and [typ] is its overall
    type. Otherwise, it is [Error type_error] where [type_error]
    describes the type error *)
and type_check_stmts ~ctx = type_check_stmts_acc ~ctx []

(** [type_check_stmts_acc ~ctx acc block] is [Ok (lst, typ)] where [lst]
    is a block of the decorated nodes of [block] appended to [acc] and
    [typ] is its overall type. Otherwise, it is [Error type_error] where
    [type_error] describes the type error *)
and type_check_stmts_acc ~ctx acc = function
  | [] -> Ok ([], `Unit)
  | s :: stmts ->
      let%bind s = type_check_stmt ~ctx s in
      let acc = s :: acc in
      if List.is_empty stmts then Ok (List.rev acc, DecNode.Stmt.typ s)
      else
        let%bind () = DecNode.Stmt.assert_unit s in
        type_check_stmts_acc ~ctx:(DecNode.Stmt.context s) acc stmts

(** [fold_decls ~ctx ds] is [Ok ctx'] where [ctx'] is [ctx] extended
    with every binding in [ds] if [ds] and [ctx] are disjoint, or
    [Error] otherwise *)
let fold_decls ~ctx ~pos =
  let f ctx (id, typ) = Context.add_var ~id ~typ ctx in
  List.fold_result ~init:ctx ~f

(** [get_fn_context ~ctx ~pos signature] is [Ok ctx'] where [ctx'] is
    the result of adding the parameters and relevant return type to
    [ctx], or [Error] otherwise *)
let get_fn_context ~ctx ~pos { id; params; types } =
  let ret = term_of_tau_list types in
  let%map fn_ctx = fold_decls ~ctx ~pos params in
  Context.with_ret ~ret fn_ctx

(** [get_sig_context ~pos ~ctx ~f s] is [Ok ctx'] where [ctx'] is the
    context after applying [f] to add the signature to the context as
    either a function declaration or function definition. Otherwise, it
    is [Error] *)
let get_sig_context ~pos ~ctx ~f { id; params; types } =
  let arg = term_of_tau_list (List.map ~f:snd params) in
  let ret = term_of_tau_list types in
  f ~id ~arg ~ret ctx

(** [type_check_function ~ctx ~pos signature block] is [Ok fn] where
    [fn] is FnDefn ([signature], [block]) decorated, or
    [Error type_error] where [type_error] describes the type error
    otherwise *)
let type_check_function ~ctx ~pos signature block =
  let%bind ctx =
    get_sig_context ~pos ~ctx ~f:Context.add_fn_defn signature
  in
  let%bind fn_ctx = get_fn_context ~ctx ~pos signature in
  let%bind block, typ = type_check_stmts ~ctx:fn_ctx block in
  let fn_defn = Decorated.Toplevel.FnDefn (signature, block) in
  match signature.types with
  | [] -> Ok (DecNode.Toplevel.make ~ctx ~pos fn_defn)
  | _ ->
      assert_void typ >>? pos >>| fun () ->
      DecNode.Toplevel.make ~ctx ~pos fn_defn

(** [type_check_global_decl ~ctx ~pos id typ] is [Ok gd] where [gd] is
    GlobalDecl ([id], [typ]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
let check_global_decl ~ctx ~pos id typ =
  let%map ctx = Context.add_var ~id ~typ ctx in
  let globdecl = Decorated.Toplevel.GlobalDecl (id, typ) in
  DecNode.Toplevel.make ~ctx ~pos globdecl

(** [type_check_global_init ~ctx ~pos id tau primitive] is [Ok gi] where
    [gi] is GlobalInit ([id], [tau], [primitive]) decorated, or
    [Error type_error] where [type_error] describes the type error
    otherwise *)
let check_global_init ~ctx ~pos id tau primitive =
  let%bind ctx = Context.add_var ~id ~typ:tau ctx in
  let p_type = type_of_primitive primitive in
  let tau_type = (tau :> expr) in
  let error () = Positioned.mismatch pos ~expect:tau_type p_type in
  let%map () = Lazy.ok_if_true ~error (Expr.equal tau_type p_type) in
  let globinit = Decorated.Toplevel.GlobalInit (id, tau, primitive) in
  DecNode.Toplevel.make ~ctx ~pos globinit

(** [type_check_defn ~ctx node] is [Ok defn] where [defn] is [node]
    decorated, or [Error type_error] where [type_error] describes the
    type error otherwise *)
let check_defn ~ctx node =
  let pos = PosNode.position node in
  match PosNode.get node with
  | FnDefn (signature, block) ->
      type_check_function ~ctx ~pos signature block
  | GlobalDecl (id, typ) -> check_global_decl ~ctx ~pos id typ
  | GlobalInit (id, tau, primitive) ->
      check_global_init ~ctx ~pos id tau primitive

(** [fold_context ~f ~ctx nodes] folds [f] over each node of [nodes],
    returning a pair [(ctx', nodes')] where [ctx'] is the decorated
    context and [nodes'] are the decorated nodes *)
let fold_context ~f ~ctx nodes =
  let fold (ctx, acc) node =
    let%map node = f ~ctx node in
    (DecNode.Toplevel.context node, node :: acc)
  in
  let init = (ctx, []) in
  List.fold_result ~init ~f:fold nodes >>| Tuple2.map_snd ~f:List.rev

(** [check_defs ~ctx defs] is [Ok lst] where [lst] is the decorated
    nodes of [defs] *)
let check_defs ~ctx defs = fold_context ~f:check_defn ~ctx defs >>| snd

(** [type_check_signature_no_params ~ctx signode] is [Ok sign] where
    [sign] is the [signode] decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
let type_check_signature ~ctx signode =
  let signature = PosNode.get signode in
  let pos = PosNode.position signode in
  let%bind ctx =
    get_sig_context ~pos ~ctx ~f:Context.add_fn_decl signature
  in
  let%map fn_ctx = fold_decls ~ctx ~pos signature.params in
  DecNode.Toplevel.make ~ctx ~pos signature

(** [fold_intf ctx sigs] is [Ok (ctx', sigs')] where [ctx'] is the
    context resulting from adding [sigs], and [sigs'] are the decorated
    nodes of [sigs]. Otherwise, it is [Error type_error] where
    [type_error] describes the type error *)
let fold_intf = fold_context ~f:type_check_signature

(** [fold_intf_map ~f ~ctx sigs] is [Ok res] where [res] is the result
    of applying [f] to a call of [fold_intf ~ctx sigs], or
    [Error type_error] where [type_error] describes the type error
    otherwise*)
let fold_intf_map ~f ~ctx sigs = fold_intf ~ctx sigs >>| f

(** [intf_context ctx sigs] is [Ok ctx'] where [ctx'] is the context
    after adding signatures in [sigs], or [Error type_error] where
    [type_error] describes the type error otherwise *)
let intf_context = fold_intf_map ~f:fst

let fold_sig ctx signode =
  let signature = PosNode.get signode in
  let pos = PosNode.position signode in
  get_sig_context ~pos ~ctx ~f:Context.add_fn_decl signature

(** [first_pass_sigs ~ctx sigs] is [Ok ctx'] where [ctx'] is [ctx]
    updated with the signature in [sigs], or [Error type_error] where
    [type_error] describes the type error, otherwise. *)
let first_pass_intf ~ctx intf =
  List.fold_result ~init:ctx ~f:fold_sig intf

(** [check_use ~find_intf ~ctx use] is [Ok use'] where [use'] is the
    decorated node of [use], or [Error type_error] where [type_error]
    describes the type error otherwise *)
let check_use ~find_intf ~ctx use =
  let id = PosNode.get use in
  let error () = Context.Error.unbound_intf id in
  let intf = find_intf (PosNode.get id) in
  let%bind intf = Lazy.of_option ~error intf in
  let%bind ctx = first_pass_intf ~ctx intf in
  let%map ctx = intf_context ~ctx intf in
  DecNode.Toplevel.of_pos_node ~ctx ~node:use id

(** [check_uses ~find_intf ~ctx uses] is [Ok (uses', ctx')] where
    [uses'] are the decorated nodes of [uses] and [ctx'] is the updated
    context from checking the relevant interface files. Otherwise, it is
    [Error type_error] where [type_error] describes the type error *)
let check_uses ~find_intf ~ctx uses =
  fold_context ~f:(check_use ~find_intf) ~ctx uses

(** [first_pass_def ctx def] is [Ok ctx'] where [ctx'] is [ctx] updated
    with the signature in [def], or [Error type_error] where
    [type_error] describes the type error, otherwise. *)
let first_pass_def ctx node =
  let pos = PosNode.position node in
  match PosNode.get node with
  | FnDefn (signature, _) ->
      get_sig_context ~ctx ~pos ~f:Context.add_fn_decl signature
  | GlobalDecl _ | GlobalInit _ -> Ok ctx

(** [first_pass_defs ~ctx defs] is [Ok ctx'] where [ctx'] is [ctx]
    updated with the signatures in [defs], or [Error type_error] where
    [type_error] describes the type error, otherwise. *)
let first_pass_defs ~ctx = List.fold_result ~init:ctx ~f:first_pass_def

(** [first_pass_source ~find_intf source] is an updated context with the
    function names signatures in [source], along with the contexts from
    what it uses. *)
let first_pass_source ~find_intf { uses; definitions } =
  let ctx = Context.empty in
  let%bind ctx, uses = check_uses ~find_intf ~ctx uses in
  let%map ctx = first_pass_defs ~ctx definitions in
  (uses, ctx)

(** [find_intf_default _] is [None] *)
let find_intf_default _ = None

let type_check_source ?(find_intf = find_intf_default) source =
  let%bind uses, ctx = first_pass_source ~find_intf source in
  let%map definitions = check_defs ~ctx source.definitions in
  let source : Decorated.Toplevel.source = { uses; definitions } in
  Decorated.Source source

(** [type_check_intf ctx sigs] is [Ok lst] where [lst] is a list of
    decorated nodes of [sigs], or [Error type_error] where [type_error]
    describes the type error otherwise *)
let type_check_intf ~ctx intf =
  let%bind ctx = first_pass_intf ~ctx intf in
  fold_intf_map ~f:snd ~ctx intf >>| Decorated.intf

let type_check ?(find_intf = find_intf_default) = function
  | Source source -> type_check_source ~find_intf source
  | Intf intf -> type_check_intf Context.empty intf

module Decorated = Decorated
module Op = Op
