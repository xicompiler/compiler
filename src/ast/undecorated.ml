open Core
open Result.Monad_infix
open Result.Let_syntax
open Util.Result
open Decorated
open Generic
open Expr
open Stmt
open Toplevel
open Util.Fn

(** [map_error ~pos result] decorates the error variant of [result] with
    the position [pos] at which it occurs *)
let map_error ~pos = Result.map_error ~f:(Position.Error.create ~pos)

(** [r >>? pos] is [map_error ~pos r] *)
let ( >>? ) r pos = map_error ~pos r

(** fold2 ~pos ~f ~init:a [b1; ...; bn] [c1; ...; cn] is
    [f (... (f (f a b1 c1) b2 c2) ...) bn cn], short circuiting on
    return of [Error _]. If the lengths of [l1] and [l2] are not equal,
    [Error CountMismatch] is returned. *)
let fold2_result ~pos =
  let unequal_lengths = Type.Error.Positioned.count_mismatch pos in
  Util.List.fold2_result ~unequal_lengths

(** [type_check_primitive ~ctx ~pos p] is [Ok expr] where [expr] is a
    decorated expression node for [p] *)
let type_check_primitive ~ctx ~pos p =
  let typ = Primitive.typeof p in
  (Primitive p, Data.Expr.create ~ctx ~typ ~pos)

(** [type_check_id ctx i] is [Ok expr] where [expr] is a decorated
    expression node for [i] if [i] is in [ctx], or [Error err] otherwise *)
let type_check_id ~ctx ~pos id =
  let%map typ = Context.find_var ~id ctx in
  (Id id, Data.Expr.create ~ctx ~typ ~pos)

(** [decorate_concat t1 t2 ~ctx ~pos] is [Ok dec], where [dec] is a ast
    decoration wrapping context [ctx], and position [pos] if [t1 = t2],
    and [Error] if [t1 =/= t2] *)
let decorate_concat t1 t2 ~ctx ~pos =
  if Type.Expr.equal t1 t2 then
    (* TODO : factor into assert fn *)
    Ok (Data.Expr.create ~ctx ~typ:t1 ~pos)
  else Error (Type.Error.Positioned.op_mismatch pos)

(** [decorate_eq data1 data2 ~ctx ~pos] is an AST decoration context
    [ctx], and position [pos] representing the expression [e1 = e2],
    where [data1] is the decoration of [e1] and [data2] is the
    decoration of [e1] *)
let decorate_eq d1 d2 ~ctx ~pos =
  let%bind expect = Data.Expr.to_tau d1 in
  let%map () = Data.Expr.assert_eq ~expect d2 in
  Data.Expr.create_bool ~ctx ~pos

(** [decorate_bop op d1 d2 ~ctx ~pos] is [Ok data], where [data] is the
    decoration of a binary operator with operation [op], left
    subexpression data [d1] and right subexpression data [d2] occuring
    in typing context [ctx] at position [pos] *)
let decorate_bop op d1 d2 ~ctx ~pos =
  let open Data.Expr in
  match (op, typ d1, typ d2) with
  | #Binop.arith, `Int, `Int -> Ok (create_int ~ctx ~pos)
  | `Add, (`Array _ as t1), (`Array _ as t2) ->
      decorate_concat t1 t2 ~ctx ~pos
  | #Binop.ord, `Int, `Int | #Binop.log, `Bool, `Bool ->
      Ok (create_bool ~ctx ~pos)
  | #Binop.eq, _, _ -> decorate_eq d1 d2 ~ctx ~pos
  | _ -> Error (Type.Error.Positioned.op_mismatch pos)

(** [decorate_uop op d ~ctx ~pos] is [Ok data], where [data] is the
    decoration of a unary operator with operation [op] and subexpression
    data [d] occuring in typing context [ctx] at position [pos] *)
let decorate_uop op d ~ctx ~pos =
  let open Data.Expr in
  match (op, typ d) with
  | `IntNeg, (`Int as typ) | `LogNeg, (`Bool as typ) ->
      Ok (create ~typ ~ctx ~pos)
  | `IntNeg, _ | `LogNeg, _ ->
      Error (Type.Error.Positioned.op_mismatch pos)

(** [decorate_index ~ctx ~pos d1 d2] is [Ok data] where [data] is a
    decoration wrapping type [`Int] with context [ctx] at position
    [pos], or [Error type_error] where [type_error] describes the type
    error otherwise *)
let decorate_index ~ctx ~pos d1 d2 =
  let open Data.Expr in
  match (typ d1, typ d2) with
  | `Array typ, `Int -> Ok (create ~ctx ~typ ~pos)
  | _, _ -> Error (Type.Error.Positioned.op_mismatch pos)

(** [decorate_term ~f ~ctx node] is a pair [(decoration, node')] where
    [decoration] is the type-checking decoration of [node] and [node']
    is the type checked node, as produced by [f ~ctx node] *)
let decorate_term ~f ~ctx node =
  let%map ((_, data) as node) = f ~ctx node in
  (node, data)

let rec type_check_expr ~ctx (e, pos) =
  match e with
  | Primitive p -> Ok (type_check_primitive ~ctx ~pos p)
  | Id id -> type_check_id ~ctx ~pos id
  | Array arr -> type_check_array ~ctx ~pos arr
  | String s -> Ok (type_check_string ~ctx ~pos s)
  | Bop (op, e1, e2) -> type_check_bop ~ctx ~pos op e1 e2
  | Uop (op, e) -> type_check_uop ~ctx ~pos op e
  | FnCall (id, es) -> type_check_fn_call ~ctx ~pos id es
  | Length node -> type_check_length ~ctx ~pos node
  | Index (e1, e2) -> type_check_index ~ctx ~pos e1 e2

(** [decorate_expr ~ctx node] is a pair [(decoration, node')] where
    [decoration] is the type-checking decoration of [node] and [node']
    is the type checked node *)
and decorate_expr ~ctx node = decorate_term ~f:type_check_expr ~ctx node

(** [type_check_exprs ~ctx ~pos ~types es] is [Ok es'] where [es'] is
    [es] decorated if each of [es] typechecks and matches with its
    corresponding type in [types], [Error CountMismatch] if the length
    of the [types] and [es] differ, and [Error err] if [err] is
    propogated during the type-checking of any of [es] prior to a
    [CountMismatch] *)
and type_check_exprs ~ctx ~pos ~types es =
  let f acc expect e =
    let%bind e, data = decorate_expr ~ctx e in
    let%map () = Data.Expr.assert_eq ~expect data in
    e :: acc
  in
  fold2_result ~f ~pos ~init:[] types es >>| List.rev

(** [type_check_array ~ctx ~pos arr] is [Ok arr'] where [arr'] is [arr]
    decorated, or [Error type_error] where [type_error] describes the
    type error otherwise *)
and type_check_array ~ctx ~pos es =
  let f = fold_array ~ctx in
  let init = ([], `Bot) in
  let%map es, typ = List.fold_result es ~init ~f in
  let typ = `Array typ in
  (Array (List.rev es), Data.Expr.create ~ctx ~typ ~pos)

(** [fold_array ctx (prev, acc) elt] is [Ok lst] where [lst] is [elt]
    decorated and appended to [acc], ensuring that its type is equal to
    that of [prev], or [Error type_error] where [type_error] describes
    the type error otherwise *)
and fold_array ~ctx (acc, prev) e =
  let%bind e, data = decorate_expr ~ctx e in
  let%map super = Data.Expr.join ~expect:prev data in
  (e :: acc, super)

(** [type_check_string ~ctx ~pos str] is [Ok str'] where [str'] is [str]
    decorated, or [Error type_error] where [type_error] describes the
    type error otherwise *)
and type_check_string ~ctx ~pos str =
  let typ = `Array `Int in
  (String str, Data.Expr.create ~ctx ~typ ~pos)

(** [type_check_bop ~ctx ~pos op e1 e2] is [Ok bop] where [bop] is Bop
    ([op], [e1], [e2]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
and type_check_bop ~ctx ~pos op e1 e2 =
  let%bind e1, d1 = decorate_expr ~ctx e1 in
  let%bind e2, d2 = decorate_expr ~ctx e2 in
  let%map data = decorate_bop op d1 d2 ~ctx ~pos in
  (Bop (op, e1, e2), data)

(** [type_check_uop ~ctx ~pos op e] is [Ok uop] where [uop] is Uop
    ([op], [e]) decorated, or [Error type_error] where [type_error]
    describes the type error otherwise *)
and type_check_uop ~ctx ~pos op e =
  let%bind e, data = decorate_expr ctx e in
  let%map data = decorate_uop op data ~ctx ~pos in
  (Uop (op, e), data)

(** [type_check_call ~ctx ~pos id es] is [Ok (es', t)] where [es'] is
    the nodes in [es] decorated and [t] is the return type of function
    [id], or [Error type_error] where [type_error] describes the type
    error otherwise *)
and type_check_call ~ctx ~pos id es =
  let%bind t1, t2 = Context.find_fn ~id ctx in
  let types = Type.Term.to_tau_list t1 in
  let%map es = type_check_exprs ~ctx ~pos ~types es in
  (es, t2)

(** [type_check_fn_call ~ctx ~pos id es] is [Ok fn] where [fn] is FnCall
    ([id], [es]) decorated, or [Error type_error] where [type_error]
    describes the type error otherwise *)
and type_check_fn_call ~ctx ~pos id es =
  let%map es, t2 = type_check_call ~ctx ~pos id es in
  let typ = Type.Term.to_expr t2 in
  let e = FnCall (id, es) in
  (e, Data.Expr.create ~ctx ~typ ~pos)

(** [type_check_length ~ctx ~pos node] is [Ok ln] where [ln] is Length
    [node] decorated, or [Error type_error] where [type_error] describes
    the type error otherwise *)
and type_check_length ~ctx ~pos e =
  let%bind e, data = decorate_expr ctx e in
  let%map () = Data.Expr.assert_array data in
  (Length e, Data.Expr.create_int ~ctx ~pos)

(** [type_check_index ~ctx ~pos e1 e2] is [Ok idx] where [idx] is Index
    ([e1], [e2]) decorated, or [Error type_error] where [type_error]
    describes the type error otherwise *)
and type_check_index ~ctx ~pos e1 e2 =
  let%bind e1, d1 = decorate_expr ctx e1 in
  let%bind e2, d2 = decorate_expr ctx e2 in
  let%map data = decorate_index ~ctx ~pos d1 d2 in
  (Index (e1, e2), data)

(** [bool_or_error_stmt ctx e] is [Ok e] if [e] is [Ok e] and [e] has
    bool type in function context [ctx] and [Error ExprMismatch]
    otherwise *)
let bool_or_error ~ctx e =
  let%bind e, data = decorate_expr ~ctx e in
  let%map () = Data.Expr.assert_bool data in
  e

(** [type_check_var_decl ~ctx ~pos id typ] is [Ok vd] where [vd] is
    VarDecl ([id], [typ]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
let type_check_var_decl ~ctx ~pos id typ =
  let%map ctx = Context.add_var ~id ~typ ctx in
  (VarDecl (id, typ), Data.Stmt.create_unit ~ctx ~pos)

(** [type_check_empty es] is [Ok \[\]] if each of [es] is [None], or
    [Error err] if any of [es] are [Some _]*)
let rec type_check_empty = function
  | [] -> Ok []
  | None :: es -> type_check_empty es
  | Some (_, pos) :: _ ->
      Error (Type.Error.Positioned.illegal_arr_decl pos)

(** [type_check_sizes ~ctx es] is [Ok \[e1; ...; em\]] if [es] is
    [Some e1; ... Some en; None ...] or [Error err] if [Some _] follows
    [None] in [es] *)
let rec type_check_sizes ~ctx = function
  | [] -> Ok []
  | None :: es -> type_check_empty es
  | Some e :: es ->
      let%bind e, data = decorate_expr ~ctx e in
      let%bind () = Data.Expr.assert_int data in
      type_check_sizes ~ctx es >>| List.cons e

(** [type_check_array_decl ~ctx ~pos id typ es] is [Ok ad] where [ad] is
    ArrayDecl ([id], [typ], [es]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
let type_check_array_decl ~ctx ~pos id typ es =
  let%bind ctx = Context.add_var ~id ~typ ctx in
  let%map es = type_check_sizes ~ctx es in
  let es = List.map ~f:Option.some es in
  (ArrayDecl (id, typ, es), Data.Stmt.create_unit ~ctx ~pos)

(** [type_check_array_decl ~ctx ~pos id e] is [Ok assn] where [assn] is
    Assign ([id], [e]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
let type_check_assign ~ctx ~pos id e =
  let%bind typ = Context.find_var ~id ctx in
  let%bind n, data = decorate_expr ~ctx e in
  let%map () = Data.Expr.assert_eq ~expect:typ data in
  (Assign (id, n), Data.Stmt.create_unit ~ctx ~pos)

(** [type_check_expr_stmt ~ctx ~pos id es] is [Ok es] where [es] is
    ExprStmt ([id], [es]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
let type_check_expr_stmt ~ctx ~pos id es =
  let%map es, _ = type_check_call ~ctx ~pos id es in
  (ExprStmt (id, es), Data.Stmt.create_unit ~ctx ~pos)

(** [type_check_var_init ~ctx ~pos id typ e] is [Ok vi] where [vi] is
    VarInit ([id], [typ], [e]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
let type_check_var_init ~ctx ~pos id typ e =
  let%bind ctx = Context.add_var ~id ~typ ctx in
  let%bind n, data = decorate_expr ~ctx e in
  let%map () = Data.Expr.assert_eq ~expect:typ data in
  (VarInit (id, typ, n), Data.Stmt.create_unit ~ctx ~pos)

(** [type_check_arr_assign ~ctx ~pos e1 e2 e3] is [Ok assn] where [assn]
    is ArrayAssign ([e1], [e2], [e3]) decorated, or [Error type_error]
    where [type_error] describes the type error otherwise *)
let type_check_arr_assign ~ctx ~pos e1 e2 e3 =
  let%bind n1, d1 = decorate_expr ~ctx e1 in
  let%bind n2, d2 = decorate_expr ~ctx e2 in
  (* TODO : clean below up using assert function *)
  match (Data.Expr.typ d1, Data.Expr.typ d2) with
  | `Array expect, `Int ->
      let%bind n3, d3 = decorate_expr ~ctx e3 in
      let%map () = Data.Expr.assert_eq ~expect d3 in
      (ArrAssign (n1, n2, n3), Data.Stmt.create_unit ~ctx ~pos)
  | _, `Int ->
      let pos1 = Data.Expr.position d1 in
      Error (Type.Error.Positioned.expected_array pos1)
  | _, got ->
      let pos2 = Data.Expr.position d2 in
      Error (Type.Error.Positioned.expr_mismatch pos2 ~expect:`Int ~got)

(** [check_decls ~ctx ~pos ds ts] is [Ok ctx'] where [ctx'] is [ctx]
    updated with variables [ds] and types [ts], or [Error type_error]
    where [type_error] describes the type error otherwise *)
let check_decls ~ctx ~pos =
  let f ctx d typ =
    match d with
    | None -> Ok ctx
    | Some (id, tau) ->
        let open Type.Error.Positioned in
        let error () = expr_mismatch pos ~expect:typ ~got:tau in
        let%bind () = Lazy.ok_if_true (Type.Tau.equal typ tau) ~error in
        Context.add_var ~id ~typ ctx
  in
  fold2_result ~pos ~f ~init:ctx

(** [type_check_multi_assign ~ctx ~pos ds id es] is [Ok mult] where
    [mult] is MultiAssign ([ds], [id], [es]) decorated, or
    [Error type_error] where [type_error] describes the type error
    otherwise *)
let type_check_multi_assign ~ctx ~pos ds id es =
  let%bind es, t2 = type_check_call ~ctx ~pos id es in
  let ts = Type.Term.to_tau_list t2 in
  let%map ctx = check_decls ~ctx ~pos ds ts in
  (MultiAssign (ds, id, es), Data.Stmt.create_unit ~ctx ~pos)

(** [type_check_return ~ctx ~pos es] is [Ok ret] where [ret] is Return
    [es] decorated, or [Error type_error] where [type_error] describes
    the type error otherwise *)
let type_check_return ~ctx ~pos es =
  let rho = Context.ret ctx in
  let types = Type.Term.to_tau_list rho in
  let%map es = type_check_exprs ~ctx ~pos ~types es in
  (Return es, Data.Stmt.create_void ~ctx ~pos)

(** [term_of_sig signature] is a [Term.t] representing the return type
    of [signature] *)
let term_of_sig = Sig.ret >> Type.Term.of_tau_list

let rec type_check_stmt ~ctx (s, pos) =
  match s with
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

(** [decorate_stmt ~ctx s] is a pair [(decoration, node')] where
    [decoration] is the type-checking decoration of [node] and [node']
    is the type checked node *)
and decorate_stmt ~ctx s = decorate_term ~f:type_check_stmt ~ctx s

(** [create_cond ~f ~ctx ~pos e s] is [Ok u] where [u] is a decorated
    unit node that carries the result of applying [f] to [e] and [s], or
    [Error type_error] where [type_error] describes the type error
    otherwise *)
and create_cond ~f ~ctx ~pos e s =
  let%bind e = bool_or_error ~ctx e in
  let%map s = type_check_stmt ~ctx s in
  (f e s, Data.Stmt.create_unit ~ctx ~pos)

(** [type_check_if ~ctx ~pos e s] is [Ok if] where [if] is If ([e], [s])
    decorated, or [Error type_error] where [type_error] describes the
    type error otherwise *)
and type_check_if ~ctx ~pos e s =
  create_cond ~f:(fun e s -> If (e, s)) ~ctx ~pos e s

(** [type_check_if_else ~ctx ~pos e s1 s2] is [Ok ifel] where [ifel] is
    IfElse ([e], [s1], [s2]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
and type_check_if_else ~ctx ~pos e s1 s2 =
  let%bind e = bool_or_error ~ctx e in
  let%bind s1, d1 = decorate_stmt ~ctx s1 in
  let%map s2, d2 = decorate_stmt ~ctx s2 in
  let typ = Data.Stmt.lub d1 d2 in
  (IfElse (e, s1, s2), Data.Stmt.create ~ctx ~pos ~typ)

(** [type_check_while ~ctx ~pos e s] is [Ok w] where [w] is While ([e],
    [s]) decorated, or [Error type_error] where [type_error] describes
    the type error otherwise *)
and type_check_while ~ctx ~pos e s =
  create_cond ~f:(fun e s -> While (e, s)) ~ctx ~pos e s

(** [type_check_pr_call ~ctx ~pos id es] is [Ok pr] where [pr] is PrCall
    ([id], [es]) decorated, or [Error type_error] where [type_error]
    describes the type error otherwise *)
and type_check_pr_call ~ctx ~pos id es =
  let%bind es, t2 = type_check_call ~ctx ~pos id es in
  let%map () = Type.assert_unit t2 >>? pos in
  (PrCall (id, es), Data.Stmt.create_unit ~ctx ~pos)

(** [type_check_block ~ctx ~pos stmts] is [Ok block] where [block] is
    Block [stmts] decorated, or [Error type_error] where [type_error]
    describes the type error otherwise *)
and type_check_block ~ctx ~pos stmts =
  let%map stmts, typ = type_check_stmts ~ctx stmts in
  (Block stmts, Data.Stmt.create ~ctx ~pos ~typ)

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
  | [] -> Ok ([], `Unit) (* Only ever matches if initial block empty *)
  | s :: stmts ->
      let%bind s, data = decorate_stmt ~ctx s in
      let acc = s :: acc in
      if List.is_empty stmts then Ok (List.rev acc, Data.Stmt.typ data)
      else
        let%bind () = Data.Stmt.assert_unit data in
        type_check_stmts_acc ~ctx:(Data.Stmt.context data) acc stmts

(** [fold_decls ~ctx ds] is [Ok ctx'] where [ctx'] is [ctx] extended
    with every binding in [ds] if [ds] and [ctx] are disjoint, or
    [Error] otherwise *)
let fold_decls ~ctx ~pos =
  let f ctx (id, typ) = Context.add_var ~id ~typ ctx in
  List.fold_result ~init:ctx ~f

(** [get_fn_context ~ctx ~pos signature] is [Ok ctx'] where [ctx'] is
    the result of adding the parameters and relevant return type to
    [ctx], or [Error] otherwise *)
let get_fn_context ~ctx ~pos signature =
  let%map fn_ctx = fold_decls ~ctx ~pos @@ Sig.params signature in
  let ret = term_of_sig signature in
  Context.with_ret ~ret fn_ctx

(** [get_sig_context ~pos ~ctx ~f s] is [Ok ctx'] where [ctx'] is the
    context after applying [f] to add the signature to the context as
    either a function declaration or function definition. Otherwise, it
    is [Error] *)
let get_sig_context ~pos ~ctx ~f s =
  let arg = Type.Term.of_tau_list @@ List.map ~f:snd @@ Sig.params s in
  let ret = term_of_sig s in
  f ~id:(Sig.name s) ~arg ~ret ctx

(** [type_check_function ~ctx ~pos signature block] is [Ok fn] where
    [fn] is FnDefn ([signature], [block]) decorated, or
    [Error type_error] where [type_error] describes the type error
    otherwise *)
let type_check_function ~ctx ~pos signature block =
  let f = Context.add_fn_defn in
  let%bind ctx = get_sig_context ~pos ~ctx ~f signature in
  let%bind fn_ctx = get_fn_context ~ctx ~pos signature in
  let%bind block, typ = type_check_stmts ~ctx:fn_ctx block in
  let fn_defn = FnDefn (signature, block) in
  match Sig.ret signature with
  | [] -> Ok (fn_defn, Data.Toplevel.create ~ctx ~pos)
  | _ ->
      let%map () = Type.assert_void typ >>? pos in
      (fn_defn, Data.Toplevel.create ~ctx ~pos)

(** [type_check_global_decl ~ctx ~pos id typ] is [Ok gd] where [gd] is
    GlobalDecl ([id], [typ]) decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
let check_global_decl ~ctx ~pos id typ =
  let%map ctx = Context.add_var ~id ~typ ctx in
  (GlobalDecl (id, typ), Data.Toplevel.create ~ctx ~pos)

(** [type_check_global_init ~ctx ~pos id tau primitive] is [Ok gi] where
    [gi] is GlobalInit ([id], [tau], [primitive]) decorated, or
    [Error type_error] where [type_error] describes the type error
    otherwise *)
let check_global_init ~ctx ~pos id tau primitive =
  let%bind ctx = Context.add_var ~id ~typ:tau ctx in
  let expect, got = (tau, Primitive.typeof primitive) in
  let error () = Type.Error.Positioned.expr_mismatch pos ~expect ~got in
  (* TODO : this is what assert_eq exists for *)
  let%map () = Lazy.ok_if_true ~error (Type.Expr.equal expect got) in
  (GlobalInit (id, tau, primitive), Data.Toplevel.create ~ctx ~pos)

(** [type_check_defn ~ctx node] is [Ok defn] where [defn] is [node]
    decorated, or [Error type_error] where [type_error] describes the
    type error otherwise *)
let check_defn ~ctx (def, pos) =
  match def with
  | FnDefn (proto, block) -> type_check_function ~ctx ~pos proto block
  | GlobalDecl (id, typ) -> check_global_decl ~ctx ~pos id typ
  | GlobalInit (id, typ, e) -> check_global_init ~ctx ~pos id typ e

(** [fold_context ~f ~ctx nodes] folds [f] over each node of [nodes],
    returning a pair [(ctx', nodes')] where [ctx'] is the decorated
    context and [nodes'] are the decorated nodes *)
let fold_context ~f ~ctx nodes =
  let fold (ctx, acc) node =
    let%map node = f ~ctx node in
    (Data.Toplevel.context @@ Entry.data node, node :: acc)
  in
  let init = (ctx, []) in
  List.fold_result ~init ~f:fold nodes >>| Tuple2.map_snd ~f:List.rev

(** [check_defs ~ctx defs] is [Ok lst] where [lst] is the decorated
    nodes of [defs] *)
let check_defs ~ctx defs = fold_context ~f:check_defn ~ctx defs >>| snd

(** [type_check_signature_no_params ~ctx signode] is [Ok sign] where
    [sign] is the [signode] decorated, or [Error type_error] where
    [type_error] describes the type error otherwise *)
let type_check_signature ~ctx (signature, pos) =
  let f = Context.add_fn_decl in
  let%bind ctx = get_sig_context ~pos ~ctx ~f signature in
  let%map _ = fold_decls ~ctx ~pos @@ Sig.params signature in
  (signature, Data.Toplevel.create ~ctx ~pos)

(** [fold_intf_map ~f ~ctx sigs] is [Ok res] where [res] is the result
    of applying [f] to a call of [fold_intf ~ctx sigs], or
    [Error type_error] where [type_error] describes the type error
    otherwise*)
let fold_intf_map ~f ~ctx sigs =
  fold_context ~f:type_check_signature ~ctx sigs >>| f

(** [fold_sig ctx signode] is [Ok ctx'] where [ctx'] is the result of
    adding the function declaration of [signode] to [ctx], or
    [Error type_error] where [type_error] describes the type error
    otherwise *)
let fold_sig ctx (signature, pos) =
  get_sig_context ~pos ~ctx ~f:Context.add_fn_decl signature

(** [check_intf ~ctx sigs] is [Ok ctx'] where [ctx'] is [ctx] updated
    with the signature in [sigs], or [Error type_error] where
    [type_error] describes the type error, otherwise. *)
let check_intf ~ctx intf =
  let%bind ctx = List.fold_result ~init:ctx ~f:fold_sig intf in
  fold_intf_map ~f:fst ~ctx intf

(** [check_use ~find_intf ~ctx use] is [Ok use'] where [use'] is the
    decorated node of [use], or [Error type_error] where [type_error]
    describes the type error otherwise *)
let check_use ~find_intf ~ctx (((name, pos) as id), _) =
  let error () = Context.Error.unbound_intf id in
  let intf = find_intf name in
  let%bind intf = Lazy.of_option ~error intf in
  let%map ctx = check_intf ~ctx intf in
  (id, Data.Toplevel.create ~ctx ~pos)

(** [check_uses ~find_intf ~ctx uses] is [Ok (uses', ctx')] where
    [uses'] are the decorated nodes of [uses] and [ctx'] is the updated
    context from checking the relevant interface files. Otherwise, it is
    [Error type_error] where [type_error] describes the type error *)
let check_uses ~find_intf ~ctx uses =
  fold_context ~f:(check_use ~find_intf) ~ctx uses

(** [first_pass_def ctx def] is [Ok ctx'] where [ctx'] is [ctx] updated
    with the signature in [def], or [Error type_error] where
    [type_error] describes the type error, otherwise. *)
let first_pass_def ctx (glob, pos) =
  match glob with
  | FnDefn (s, _) -> get_sig_context ~ctx ~pos ~f:Context.add_fn_decl s
  | GlobalDecl _ | GlobalInit _ -> Ok ctx

(** [first_pass_defs ~ctx defs] is [Ok ctx'] where [ctx'] is [ctx]
    updated with the signatures in [defs], or [Error type_error] where
    [type_error] describes the type error, otherwise. *)
let first_pass_defs ~ctx = List.fold_result ~init:ctx ~f:first_pass_def

(** [first_pass_source ~find_intf source] is an updated context with the
    function names signatures in [source], along with the contexts from
    what it uses. *)
let first_pass_source ~find_intf src =
  let ctx = Context.empty in
  let%bind ctx, uses = check_uses ~find_intf ~ctx @@ Source.uses src in
  let%map ctx = first_pass_defs ~ctx @@ Source.defs src in
  (uses, ctx)

(** [find_intf_default _] is [None] *)
let find_intf_default _ = None

let type_check_source ~find_intf source =
  let%bind uses, ctx = first_pass_source ~find_intf source in
  let%map defs = check_defs ~ctx @@ Source.defs source in
  Source (Source.create ~uses ~defs)

(** [type_check_intf sigs] is [Ok lst] where [lst] is a list of
    decorated nodes of [sigs], or [Error type_error] where [type_error]
    describes the type error otherwise *)
let type_check_intf intf =
  let%bind ctx = check_intf ~ctx:Context.empty intf in
  fold_intf_map ~f:snd ~ctx intf >>| Generic.intf

let type_check ?(find_intf = find_intf_default) = function
  | Source source -> type_check_source ~find_intf source
  | Intf intf -> type_check_intf intf

type find_intf = string -> Position.t Toplevel.intf option

module Data = struct
  type expr = Position.t
  type stmt = Position.t
  type toplevel = Position.t
end

include Types.Make (Data)
