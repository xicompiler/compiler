open Core
open Int64
open Subtype
module PosNode = Node.Position
module DecNode = Context.Node.Decorated
open Ast.Op
open Ast.Decorated
open Expr
open Stmt
open Toplevel
open Primitive
open Type
open IrGensym
module Map = String.Map
module Set = String.Set

type expr =
  [ expr Subtype.expr
  | expr Subtype.call
  | `ESeq of stmt * expr
  ]

and stmt =
  [ expr Subtype.cjump2
  | `Seq of stmt list
  ]

type toplevel =
  [ `Func of label * stmt list
  | `Data of label * int64 list
  ]

type dest = expr Subtype.dest

(** [seq lst] is [`Seq lst] *)
let seq lst = `Seq lst

(** [empty] is an empty sequence of statements *)
let empty : stmt = `Seq []

(** [exists_expr ~f e] is [true] iff [e] contains a node satisfying
    [f e] *)
let rec exists_expr ~f : expr -> bool = function
  | e when f e -> true
  | `Bop (_, e1, e2) -> exists_expr2 ~f e1 e2
  | `Call (_, e, es) -> exists_call ~f e es
  | `ESeq (s, e) -> exists_stmt ~f s || exists_expr ~f e
  | `Mem e -> exists_expr ~f e
  | `Const _ | `Name _ | `Temp _ -> false

(** [exists_call ~f e es] is [true] iff [`Call (e, es)] contains a node
    [e] satisfying [f e] *)
and exists_call ~f e es = exists_exprs ~f (e :: es)

(** [exists_expr2 e1 e2] is [true] iff either there exists a node [e] in
    [e1] or [e2] satisfying [f e]*)
and exists_expr2 ~f e1 e2 = exists_expr ~f e1 || exists_expr ~f e2

(** [exists_exprs es] is [true] iff any expression of [es] contains a
    [`Mem] node *)
and exists_exprs ~f es = List.exists ~f:(exists_expr ~f) es

(** [stmt_has_mem s] is [true] iff statement [s] contains a [`Mem] node *)
and exists_stmt ~f = function
  | `CJump (e, _, _) | `Jump e -> exists_expr ~f e
  | `Move (e1, e2) -> exists_expr2 ~f (e1 :> expr) e2
  | `Label _ -> false
  | `Call (_, e, es) -> exists_call ~f e es
  | `Return es -> exists_exprs ~f es
  | `Seq seq -> List.exists ~f:(exists_stmt ~f) seq

(** [def_expr ~init e] is the union of [init] and the temps modified by
    [e] *)
let rec def_expr ~init : expr -> String.Set.t = function
  | `Bop (_, e1, e2) -> def_expr2 ~init e1 e2
  | `Call (_, e, es) -> def_call ~init e es
  | `ESeq (s, e) -> def_expr ~init:(def_stmt ~init s) e
  | `Mem e -> def_expr ~init e
  | `Temp _ | `Const _ | `Name _ -> init

(** [def_call ~init e es] the union of [init] and all temps modified in
    [`Call (e, es)] *)
and def_call ~init e es = def_exprs ~init (e :: es)

(** [def_expr2 ~init e1 e2] is the union of [init], the temps modified
    by [e1], and the temps modified by [e2]*)
and def_expr2 ~init e1 e2 = def_expr ~init:(def_expr ~init e1) e2

(** [def_exprs ~init es] is the union of [init] and each of the sets of
    temps modified by each expression in [es] *)
and def_exprs ~init es =
  List.fold es ~init ~f:(fun acc -> def_expr ~init:acc)

(** [def_stmt ~init s] is the union of [init] and every temp used by [s] *)
and def_stmt ~init = function
  | `CJump (e, _, _) | `Jump e -> def_expr ~init e
  | `Move (`Temp t, e) -> def_expr ~init:(String.Set.add init t) e
  | `Move (e1, e2) -> def_expr2 ~init (e1 :> expr) e2
  | `Label _ -> init
  | `Call (_, e, es) -> def_call ~init e es
  | `Return es -> def_exprs ~init es
  | `Seq seq -> def_stmts ~init seq

(** [def_stmts ~init stmts] is the union of [init] and every temp
    modified in [stmts] *)
and def_stmts ~init stmts =
  List.fold stmts ~init ~f:(fun acc -> def_stmt ~init:acc)

(** [use_expr ~init e] is the union of [init] and the temps used in [e] *)
let rec use_expr ~init : expr -> String.Set.t = function
  | `Bop (_, e1, e2) -> use_expr2 ~init e1 e2
  | `Call (_, e, es) -> use_call ~init e es
  | `ESeq (s, e) -> use_expr ~init:(use_stmt ~init s) e
  | `Temp t -> String.Set.add init t
  | `Mem e -> use_expr ~init e
  | `Const _ | `Name _ -> init

(** [use_call ~init e es] the union of [init] and all temps used in
    [`Call (e, es)] *)
and use_call ~init e es = use_exprs ~init (e :: es)

(** [use_expr2 ~init e1 e2] is the union of [init], the temps used by
    [e1], and the temps used by [e2]*)
and use_expr2 ~init e1 e2 = use_expr ~init:(use_expr ~init e1) e2

(** [use_exprs ~init es] is the union of [init] and each of the sets of
    temps used by each expression in [es] *)
and use_exprs ~init es =
  List.fold es ~init ~f:(fun acc -> use_expr ~init:acc)

(** [use_stmt ~init s] is the union of [init] and every temp used by [s] *)
and use_stmt ~init = function
  | `CJump (e, _, _) | `Jump e | `Move (`Temp _, e) -> use_expr ~init e
  | `Move (e1, e2) -> use_expr2 ~init (e1 :> expr) e2
  | `Label _ -> init
  | `Call (_, e, es) -> use_call ~init e es
  | `Return es -> use_exprs ~init es
  | `Seq seq -> use_stmts ~init seq

(** [use_stmts ~init stmts] is the union of [init] and every temp used
    in [stmts] *)
and use_stmts ~init stmts =
  List.fold stmts ~init ~f:(fun acc -> use_stmt ~init:acc)

(** [has_mem e] is [true] iff [e] contains a [`Mem _] node *)
let has_mem = exists_expr ~f:(function `Mem _ -> true | _ -> false)

(** [disjoint ~use ~def] is [true] if the temps used by [use] and the
    temps defined by [def] are disjoint *)
let disjoint ~use ~def =
  let use = use_expr ~init:String.Set.empty use in
  let def = def_expr ~init:String.Set.empty def in
  String.Set.are_disjoint use def

let commute e1 e2 =
  (not (has_mem e1 && has_mem e2)) && disjoint ~use:e1 ~def:e2

(** [xi_alloc] is the xi function for allocating space *)
let xi_alloc = `Name "_xi_alloc"

(** [xi_out_of_bounds] is the xi function for an out-of-bounds array
    index *)
let xi_out_of_bounds = `Name "_xi_out_of_bounds"

let ir_pr_call name = `Call (0, name, [])

(** [to_addr n] is [n] converted to a memory address *)
let to_addr n = (8L * n) + 8L

open Infix

(** [index_addr p m] is [p + m * 8] as an IR expression *)
let index_addr p m = p + (m * eight)

(** [index e1 e2] is the array index expression [e1\[e2\]] *)
let index e1 e2 = !(index_addr e1 e2)

(** [length e] is the length of the array beginning at pointer [e] *)
let length e = !(e - eight)

(** [check_bounds e1 e2] is jumps to an error location of [e2] is not a
    valid index for [e1], or does nothing otherwise ]*)
let check_bounds ~gensym e1 e2 =
  let lok, lerr = Label.fresh2 gensym in
  [
    `CJump (e2 <? length e1, lok, lerr);
    `Label lerr;
    ir_pr_call xi_out_of_bounds;
    `Label lok;
  ]

(** [encode_tau t] is [t] encoded for function mangling *)
let rec encode_tau = function
  | `Int -> "i"
  | `Bool -> "b"
  | `Array t -> "a" ^ encode_tau t
  | `Poly -> failwith "Unexpected <poly>"

(** [encode_expr t] is [t] encoded for function mangling *)
let encode_expr = function
  | `Tuple ts ->
      let len = ts |> List.length |> string_of_int in
      let types = ts |> List.map ~f:encode_tau |> String.concat in
      "t" ^ len ^ types
  | #Tau.t as t -> encode_tau t

(** [encode_term t] is [t] encoded for function mangling *)
let encode_term = function
  | `Unit -> "p"
  | #Expr.t as t -> encode_expr t

(** [encode_name s] is [s] encoded for function mangling *)
let encode_name = String.substr_replace_all ~pattern:"_" ~with_:"__"

(** [encode_args args] is [args] encoded for function mangling *)
let encode_args args = args |> List.map ~f:encode_expr |> String.concat

(** [mangle_fmt] is the format used to mangle identifiers *)
let mangle_fmt = format_of_string "_I%s_%s%s"

(** [mangle id ctx] is the mangled function name of [id] in context
    [ctx] *)
let mangle id ~ctx =
  let arg, ret = Context.find_fn_exn ~id ctx in
  let name = id |> PosNode.get |> encode_name in
  let return = encode_term ret in
  let args = arg |> tau_list_of_term |> encode_args in
  Printf.sprintf mangle_fmt name return args

(** [num_returns id ctx] is the number of values that the function [id]
    returns. *)
let num_returns id ~ctx =
  let arg, ret = Context.find_fn_exn ~id ctx in
  ret |> tau_list_of_term |> List.length

let rec factor_expr ~gensym ~map enode =
  match DecNode.Expr.get enode with
  | Array arr -> factor_arr ~gensym ~map arr
  | String s -> factor_string ~gensym ~map s
  | Bop (op, e1, e2) -> factor_bop ~gensym ~map e1 e2
  | Uop (op, e) -> factor_uop ~gensym ~map e
  | FnCall (id, es) -> factor_fn_call ~gensym ~map es
  | Length e -> factor_length ~gensym ~map e
  | Index (e1, e2) -> factor_index ~gensym ~map e1 e2
  | Primitive _ | Id _ -> map

and factor_es ~gensym ~map es =
  let f m expr = factor_expr ~gensym ~map:m expr in
  List.fold ~f ~init:map es

and factor_arr ~gensym ~map arr = factor_es ~gensym ~map arr

and factor_string ~gensym ~map s =
  let g = Global.fresh gensym in
  match Map.add ~key:s ~data:g map with `Ok m -> m | _ -> map

and factor_bop ~gensym ~map e1 e2 =
  let map = factor_expr ~gensym ~map e1 in
  factor_expr ~gensym ~map e2

and factor_uop ~gensym ~map e = factor_expr ~gensym ~map e
and factor_fn_call ~gensym ~map es = factor_es ~gensym ~map es
and factor_length ~gensym ~map e = factor_expr ~gensym ~map e

and factor_index ~gensym ~map e1 e2 =
  let map = factor_expr ~gensym ~map e1 in
  factor_expr ~gensym ~map e2

and factor_e_options ~gensym ~map es =
  es |> List.filter_opt |> factor_es ~gensym ~map

let rec factor_stmt ~gensym ~map snode =
  match DecNode.Stmt.get snode with
  | If (e, s) -> factor_if ~gensym ~map e s
  | IfElse (e, s1, s2) -> factor_if_else ~gensym ~map e s1 s2
  | While (e, s) -> factor_while ~gensym ~map e s
  | VarDecl (_, _) -> map
  | ArrayDecl (id, typ, es) -> factor_array_decl ~gensym ~map es
  | Assign (id, e) -> factor_assign ~gensym ~map e
  | ArrAssign (e1, e2, e3) -> factor_arr_assign ~gensym ~map e1 e2 e3
  | ExprStmt (id, es) -> factor_expr_stmt ~gensym ~map es
  | VarInit (id, typ, e) -> factor_var_init ~gensym ~map e
  | MultiAssign (ds, id, es) -> factor_multi_assign ~gensym ~map es
  | PrCall (id, es) -> factor_pr_call ~gensym ~map es
  | Return es -> factor_return ~gensym ~map es
  | Block stmts -> factor_block ~gensym ~map stmts

and factor_if ~gensym ~map e s =
  let map = factor_expr ~gensym ~map e in
  factor_stmt ~gensym ~map s

and factor_if_else ~gensym ~map e s1 s2 =
  let map = factor_expr ~gensym ~map e in
  let map = factor_stmt ~gensym ~map s1 in
  factor_stmt ~gensym ~map s2

and factor_while ~gensym ~map e s =
  let map = factor_expr ~gensym ~map e in
  factor_stmt ~gensym ~map s

and factor_array_decl ~gensym ~map es = factor_e_options ~gensym ~map es
and factor_assign ~gensym ~map e = factor_expr ~gensym ~map e

and factor_arr_assign ~gensym ~map e1 e2 e3 =
  let map = factor_expr ~gensym ~map e1 in
  let map = factor_expr ~gensym ~map e2 in
  factor_expr ~gensym ~map e3

and factor_expr_stmt ~gensym ~map es = factor_es ~gensym ~map es
and factor_var_init ~gensym ~map e = factor_expr ~gensym ~map e
and factor_multi_assign ~gensym ~map es = factor_es ~gensym ~map es
and factor_pr_call ~gensym ~map es = factor_es ~gensym ~map es
and factor_return ~gensym ~map es = factor_es ~gensym ~map es

and factor_stmts ~gensym ~map stmts =
  let f m stmt = factor_stmt ~gensym ~map:m stmt in
  List.fold ~f ~init:map stmts

and factor_block ~gensym ~map stmts = factor_stmts ~gensym ~map stmts

let factor_defn ~gensym ~map def =
  match DecNode.Toplevel.get def with
  | FnDefn (signature, block) -> factor_stmts ~gensym ~map block
  | _ -> map

let factor_definitions ~gensym ~map defns =
  let f m defn = factor_defn ~gensym ~map:m defn in
  List.fold ~f ~init:map defns

let global_defn ~gensym ~set def =
  match DecNode.Toplevel.get def with
  | GlobalInit (id, _, _) -> Set.add set (PosNode.get id)
  | _ -> set

let global_definitions ~gensym ~set defns =
  let f s defn = global_defn ~gensym ~set:s defn in
  List.fold ~f ~init:set defns

(** [translate_primitive p] is the mir representation of primitive [p] *)
let translate_primitive = function
  | `Int i -> i
  | `Bool b -> b |> Bool.to_int |> Int64.of_int
  | `Char c -> c |> Uchar.to_scalar |> Int64.of_int

(** [translate_primitive id] is the mir representation of [id] *)
let translate_id ~set id =
  let id = PosNode.get id in
  if Set.mem set id then `Mem (`Name id) else `Temp id

(** [alloc space] is the mir representation of a call to allocate space *)
let alloc space = `Call (1, xi_alloc, [ space ])

(** [alloc_array len] is a [`Call] to [alloc] requesting [8 * len + 8]
    bytes of memory *)
let alloc_array len = alloc (index_addr eight len)

(** [while_seq c s lh lt lf] is a while sequence with control statement
    [c], body [s], header label [lh], true label [lt], and false label
    [lf] *)
let while_seq c s lh lt lf : stmt =
  `Seq [ `Label lh; c; `Label lt; s; `Jump (`Name lh); `Label lf ]

(** [while_stmt ~gensym e s] is a while stmt with conditional [e] and
    body [s] *)
let while_stmt ~gensym e s =
  let lh, lt, lf = Label.fresh3 gensym in
  let c = `CJump (e, lt, lf) in
  while_seq c s lh lt lf

(** [while_lt ~gensym e1 e2 s] is a while stmt with condition [e1 < e2]
    and body [s], followed by increment to [e1] *)
let while_lt ~gensym e1 e2 s : stmt =
  let guard = (e1 :> expr) < e2 in
  let body = `Seq [ s; e1 := (e1 :> expr) + one ] in
  `Seq [ e1 := zero; while_stmt ~gensym guard body ]

(** [translate_expr enode] is the mir representation of expression node
    [enode] *)
let rec translate_expr ~gensym ~map ~set enode =
  let ctx = DecNode.Expr.context enode in
  match DecNode.Expr.get enode with
  | Primitive p -> `Const (translate_primitive p)
  | Id id -> translate_id ~set id
  | Array arr -> translate_arr ~gensym ~map ~set arr
  | String s -> translate_string ~gensym ~map ~set s
  | Bop (op, e1, e2) -> translate_bop ~gensym ~map ~set op e1 e2
  | Uop (op, e) -> translate_uop ~gensym ~map ~set op e
  | FnCall (id, es) ->
      (translate_call ~gensym ~map ~set ~ctx id es :> expr)
  | Length e -> translate_length ~gensym ~map ~set e
  | Index (e1, e2) -> translate_index ~gensym ~map ~set e1 e2

(** [translate_exprs es] is [es], each of which translated to IR *)
and translate_exprs ~gensym ~map ~set es =
  List.map ~f:(translate_expr ~gensym ~map ~set) es

(** [translate_arr arr] is the mir representation of [arr] *)
and translate_arr ~gensym ~map ~set arr =
  arr |> translate_exprs ~gensym ~map ~set |> translate_arr_lst ~gensym

(** [translate_arr_lst lst] is the mir representation of a list [lst] *)
and translate_arr_lst ~gensym lst =
  let n = Util.List.length lst in
  let tm = Temp.fresh gensym in
  let f (len, acc) e =
    let move = !(tm + `Const (to_addr len)) := e in
    (succ len, move :: acc)
  in
  let add_elts =
    lst |> List.fold ~f ~init:(Int64.zero, []) |> snd |> List.rev
  in
  let move = tm := alloc (`Const (to_addr n)) in
  let add_len = !tm := `Const n in
  `ESeq (`Seq (move :: add_len :: add_elts), tm + eight)

(** [translate_string str] is the mir representation of [str] *)
and translate_string ~gensym ~map ~set str =
  if Map.mem map str then
    let global = Map.find_exn map str in
    let t1, t2 = Temp.fresh2 gensym in
    `ESeq (`Seq [ t2 := `Name global; t1 := t2 + eight ], t1)
  else
    let f s = `Const (s |> int_of_char |> Int64.of_int) in
    str |> String.to_list_rev |> List.rev_map ~f
    |> translate_arr_lst ~gensym

(** [translate_uop uop e] is the mir representation of unary operator
    expression [uop e] *)
and translate_uop ~gensym ~map ~set uop e =
  match uop with
  | `IntNeg -> translate_int_neg ~gensym ~map ~set e
  | `LogNeg -> e |> translate_expr ~gensym ~map ~set |> log_neg

(** [translate_int_neg e] is the mir representation of an integer
    negation of [e] *)
and translate_int_neg ~gensym ~map ~set e =
  match DecNode.Expr.get e with
  | Primitive (`Int i) -> `Const ~-i
  | _ -> `Bop (`Minus, zero, translate_expr ~gensym ~map ~set e)

(** [translate_call ctx id es] is the mir representation of a function
    call with function id [id], arguments [es], and context [ctx] *)
and translate_call ~gensym ~map ~set ~ctx id es : expr Subtype.call =
  let name = mangle id ~ctx in
  let rets = num_returns id ~ctx in
  `Call (rets, `Name name, translate_exprs ~gensym ~map ~set es)

(** [translate_bop bop e1 e2] is the mir representation of binary
    operator expression [bop e1 e2] *)
and translate_bop ~gensym ~map ~set bop e1 e2 =
  match (bop, DecNode.Expr.get e1, DecNode.Expr.get e2) with
  | `And, _, _ -> translate_and ~gensym ~map ~set e1 e2
  | `Or, _, _ -> translate_or ~gensym ~map ~set e1 e2
  | `Plus, _, _ when Expr.is_array (DecNode.Expr.typ e1) ->
      translate_concat ~gensym ~map ~set e1 e2
  | #Ast.Op.binop, _, _ ->
      let e1 = translate_expr ~gensym ~map ~set e1 in
      let e2 = translate_expr ~gensym ~map ~set e2 in
      `Bop (Op.coerce bop, e1, e2)

(** [translate_concat e1 e2] is the mir representation of array
    concatenation [e1 @ e2] *)
and translate_concat ~gensym ~map ~set e1 e2 : expr =
  let t1, len1, ti = Temp.fresh3 gensym in
  let t2, len2, tj = Temp.fresh3 gensym in
  let total, base, ptr = Temp.fresh3 gensym in
  let start = Temp.fresh gensym in
  let body1 = index ptr ti := index t1 ti in
  let body2 = index start tj := index t2 tj in
  `ESeq
    ( `Seq
        [
          t1 := translate_expr ~gensym ~map ~set e1;
          t2 := translate_expr ~gensym ~map ~set e2;
          len1 := length t1;
          len2 := length t2;
          total := len1 + len2;
          base := alloc_array total;
          !base := total;
          ptr := base + eight;
          while_lt ~gensym ti len1 body1;
          start := index_addr ptr len1;
          while_lt ~gensym tj len2 body2;
        ],
      ptr )

(** [translate_and e1 e2] is the mir representation of [e1 and e2] *)
and translate_and ~gensym ~map ~set e1 e2 =
  let x = Temp.fresh gensym in
  let l1, l2, lf = Label.fresh3 gensym in
  `ESeq
    ( `Seq
        [
          x := zero;
          translate_control ~gensym ~map ~set e1 l1 lf;
          `Label l1;
          translate_control ~gensym ~map ~set e2 l2 lf;
          `Label l2;
          x := one;
          `Label lf;
        ],
      x )

(** [translate_or e1 e2] is the mir representation of [e1 or e2] *)
and translate_or ~gensym ~map ~set e1 e2 =
  let x = Temp.fresh gensym in
  let l1, l2, lt = Label.fresh3 gensym in
  `ESeq
    ( `Seq
        [
          x := one;
          translate_control ~gensym ~map ~set e1 lt l1;
          `Label l1;
          translate_control ~gensym ~map ~set e2 lt l2;
          `Label l2;
          x := zero;
          `Label lt;
        ],
      x )

(** [translate_length e] is the mir representation of a length function
    call with argument [e] *)
and translate_length ~gensym ~map ~set e =
  let open Infix in
  !(translate_expr ~gensym ~map ~set e - eight)

(** [translate_index e1 e2] is the mir representation of an indexing of
    [e1] at position [e2] *)
and translate_index ~gensym ~map ~set e1 e2 =
  let ta, ti = Temp.fresh2 gensym in
  let get_ptr = ta := translate_expr ~gensym ~map ~set e1 in
  let get_idx = ti := translate_expr ~gensym ~map ~set e2 in
  let bounds_check = check_bounds ~gensym ta ti in
  `ESeq (`Seq (get_ptr :: get_idx :: bounds_check), index ta ti)

(** [translate_stmt snode] is the mir representation of statement node
    [snode] *)
and translate_stmt ~gensym ~map ~set snode =
  let ctx = DecNode.Stmt.context snode in
  match DecNode.Stmt.get snode with
  | If (e, s) -> translate_if ~gensym ~map ~set e s
  | IfElse (e, s1, s2) -> translate_if_else ~gensym ~map ~set e s1 s2
  | While (e, s) -> translate_while ~gensym ~map ~set e s
  | VarDecl _ -> empty
  | ArrayDecl (id, _, es) ->
      translate_array_decl ~gensym ~map ~set id es
  | Assign (id, e) -> translate_assign ~gensym ~map ~set id e
  | ArrAssign (e1, e2, e3) ->
      translate_arr_assign ~gensym ~map ~set e1 e2 e3
  | ExprStmt (id, es) | PrCall (id, es) ->
      (translate_call ~gensym ~map ~set ~ctx id es :> stmt)
  | VarInit (id, _, e) -> translate_assign ~gensym ~map ~set id e
  | MultiAssign (ds, id, es) ->
      translate_multi_assign ~gensym ~map ~set ~ctx ds id es
  | Return es -> translate_return ~gensym ~map ~set es
  | Block stmts -> translate_block ~gensym ~map ~set stmts

(** [translate_if_stmt e s] is [stmts, f] where [stmts] is the first
    three IR instructions in if and if else statements, reversed, and
    [f] is the false label *)
and translate_if_stmt ~gensym ~map ~set e s =
  let t, f = Label.fresh2 gensym in
  let s = translate_stmt ~gensym ~map ~set s in
  ([ s; `Label t; translate_control ~gensym ~map ~set e t f ], f)

(** [translate_if e s] is the mir representation of an if statement with
    condition [e] and body [s] *)
and translate_if ~gensym ~map ~set e s =
  let stmts, f = translate_if_stmt ~gensym ~map ~set e s in
  stmts |> List.cons (`Label f) |> List.rev |> seq

(** [translate_if_else e s1 s2] is the mir representation of an if-else
    statement with condition [e] and statements [s1] and [s2] *)
and translate_if_else ~gensym ~map ~set e s1 s2 =
  let l_end = Label.fresh gensym in
  let stmts, f = translate_if_stmt ~gensym ~map ~set e s1 in
  stmts
  |> List.cons (`Jump (`Name l_end))
  |> List.cons (`Label f)
  |> List.cons (translate_stmt ~gensym ~map ~set s2)
  |> List.cons (`Label l_end)
  |> List.rev |> seq

(** [translate_while e s] is the mir representation of a while loop with
    condition [e] and loop body [s] *)
and translate_while ~gensym ~map ~set e s =
  let lh, lt, lf = Label.fresh3 gensym in
  let c = translate_control ~gensym ~map ~set e lt lf in
  let s' = translate_stmt ~gensym ~map ~set s in
  while_seq c s' lh lt lf

(** [translate_arr_assign_simple ta ti e] is the assignment of [e] to
    array [ta] at position [ti] *)
and translate_arr_assign_simple ~gensym ~map ~set ta ti e =
  let move = index ta ti := translate_expr ~gensym ~map ~set e in
  `Seq [ `Seq (check_bounds ~gensym ta ti); move ]

(** [move_of_expr ~gensym ~map e] is a pair [(mov, t)] where [t] is a
    fresh temporary and [mov] is a move statement moving the translation
    of [e] into [t] *)
and move_of_expr ~gensym ~map ~set e =
  let t = Temp.fresh gensym in
  (`Move (t, translate_expr ~gensym ~map ~set e), t)

(** [moves_of_exprs ~gensym \[e1; ...; en\]] is a pair
    [(moves, \[t1; ...; tn\])] where each [ti] is fresh and [moves] is a
    sequence of statements moving the translation of each of [ei] into
    [ti] *)
and moves_of_exprs ~gensym ~map ~set es =
  let f (moves, ts) e =
    let move, t = move_of_expr ~gensym ~map ~set e in
    (move :: moves, t :: ts)
  in
  let moves, ts = List.fold ~init:([], []) ~f es in
  (`Seq (List.rev moves), List.rev ts)

(** Same as [moves_of_exprs_opt], but takes in a list of optional
    expressions. [None] values are ignored. *)
and moves_of_exprs_opt ~gensym ~map ~set es =
  es |> List.filter_opt |> moves_of_exprs ~gensym ~map ~set

(** [translate_array_decl_helper name es] is the mir representation of
    an array declaration for [name] with lengths [es] *)
and translate_array_decl_helper ~gensym name = function
  | t :: ts ->
      let base, ptr = Temp.fresh2 gensym in
      let ti, nested_name = Temp.fresh2 gensym in
      let nested =
        `Seq
          [
            translate_array_decl_helper ~gensym nested_name ts;
            index ptr ti := nested_name;
          ]
      in
      `Seq
        [
          base := alloc_array t;
          !base := t;
          ptr := base + eight;
          name := ptr;
          while_lt ~gensym ti t nested;
        ]
  | [] -> empty

(** [translate_array_decl id es] is the mir representation of an array
    declaration for variable [id] with lengths [es] *)
and translate_array_decl ~gensym ~map ~set id es =
  let name = `Temp (PosNode.get id) in
  let moves, ts = moves_of_exprs_opt ~gensym ~map ~set es in
  let decl = translate_array_decl_helper ~gensym name ts in
  `Seq [ moves; decl ]

(** [translate_assign id e] is the mir representation of an assignment
    of [e] to [id] *)

and translate_assign ~gensym ~map ~set id e =
  let name = translate_id ~set id in
  name := translate_expr ~gensym ~map ~set e

(** [translate_arr_assign e1 e2 e3] is the mir representation of a array
    assignment statement with array reference [e1], position [e2], and
    value [e3] *)
and translate_arr_assign ~gensym ~map ~set e1 e2 e3 =
  let ta, ti = Temp.fresh2 gensym in
  `Seq
    [
      ta := translate_expr ~gensym ~map ~set e1;
      ti := translate_expr ~gensym ~map ~set e2;
      translate_arr_assign_simple ~gensym ~map ~set ta ti e3;
    ]

(** [translate_multi_assign ctx ds id es] is the mir representation of a
    multi-assignment with declarations [ds], and function [id], and
    arguments [es] *)
and translate_multi_assign ~gensym ~map ~set ~ctx ds id es =
  let name = mangle id ~ctx in
  let rets = num_returns id ~ctx in
  let expr_lst = translate_exprs ~gensym ~map ~set es in
  let call = `Call (rets, `Name name, expr_lst) in
  let f (rv, acc) = function
    | None -> (Int.succ rv, acc)
    | Some (v, _) ->
        let t = Printf.sprintf "_RV%d" rv in
        let name = translate_id ~set v in
        let move = name := `Temp t in
        (Int.succ rv, move :: acc)
  in
  let assign = ds |> List.fold ~f ~init:(1, []) |> snd |> List.rev in
  `Seq (call :: assign)

(** [translate_return es] is the mir representation of a return
    statement with expressions [es] *)
and translate_return ~gensym ~map ~set es =
  `Return (translate_exprs ~gensym ~map ~set es)

(** [translate_block stmts] is the mir representation of a statement
    block with statements [stmts] *)
and translate_block ~gensym ~map ~set stmts =
  `Seq (List.map ~f:(translate_stmt ~gensym ~map ~set) stmts)

(** [translate_control enode t f] translates booleans to control flow *)
and translate_control ~gensym ~map ~set enode t f =
  match DecNode.Expr.get enode with
  | Primitive (`Bool b) -> `Jump (`Name (if b then t else f))
  | Uop (`LogNeg, e) -> translate_control ~gensym ~map ~set e f t
  | Bop (`And, e1, e2) ->
      let label = Label.fresh gensym in
      `Seq
        [
          translate_control ~gensym ~map ~set e1 label f;
          `Label label;
          translate_control ~gensym ~map ~set e2 t f;
        ]
  | Bop (`Or, e1, e2) ->
      let label = Label.fresh gensym in
      `Seq
        [
          translate_control ~gensym ~map ~set e1 t label;
          `Label label;
          translate_control ~gensym ~map ~set e2 t f;
        ]
  | _ -> `CJump (translate_expr ~gensym ~map ~set enode, t, f)

(** [returns_unit ctx id] is true if the function associated with [id]
    in [ctx] has a return of type Unit, false otherwise *)
let returns_unit ~ctx id =
  match Context.find_fn_exn ~id ctx with _, `Unit -> true | _ -> false

(** [translate_fn_defn sign] is the mir representation of a function
    definition with signature [sign] *)
let translate_fn_defn
    ~gensym
    ~map
    ~set
    ~ctx
    ({ id; params } : Ast.signature)
    block =
  let name = mangle id ~ctx in
  let f (arg, acc) ((param, _) : Ast.decl) : int * stmt list =
    let t = Printf.sprintf "_ARG%d" arg in
    let move = `Temp (PosNode.get param) := `Temp t in
    (Int.succ arg, move :: acc)
  in
  let moves =
    params |> List.fold ~f ~init:(1, []) |> snd |> List.rev |> seq
  in
  let block = translate_block ~gensym ~map ~set block in
  let body =
    if returns_unit ~ctx id then
      [ moves; block; translate_return ~gensym ~map ~set [] ]
    else [ moves; block ]
  in
  `Func (name, body)

(** [translate_global_init id typ p] is the mir representation of a
    global initialization of [id] with type [typ] and primitive [p] as
    its value *)
let translate_global_init id p =
  `Data (PosNode.get id, [ translate_primitive p ])

(** [translate_defn def] is the mir representation of source toplevel
    definition [def] *)
let translate_defn ~gensym ~map ~set def =
  match DecNode.Toplevel.get def with
  | FnDefn (signature, block) ->
      let ctx = DecNode.Toplevel.context def in
      Some
        (translate_fn_defn ~gensym ~map ~set ~ctx signature block
          :> toplevel)
  | GlobalDecl _ -> None
  | GlobalInit (id, _, p) -> Some (translate_global_init id p)

let data_of_string id s =
  let f str = str |> int_of_char |> Int64.of_int in
  let lst = s |> String.to_list_rev |> List.rev_map ~f in
  `Data (id, (lst |> List.length |> Int64.of_int) :: lst)

let get_data_from_map map =
  let f ~key ~data acc = data_of_string data key :: acc in
  Map.fold ~f ~init:[] map

let translate ~gensym { uses; definitions } =
  let map = factor_definitions ~gensym ~map:Map.empty definitions in
  let set = global_definitions ~gensym ~set:Set.empty definitions in
  let globals = get_data_from_map map in
  globals
  @ List.filter_map ~f:(translate_defn ~gensym ~map ~set) definitions
