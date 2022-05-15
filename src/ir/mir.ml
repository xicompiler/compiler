open Core
open Subtype
open Primitive
open Type
open Util.Fn
open Ast.Expr
open Ast.Stmt
open Ast.Toplevel
open Ast.Decorated
open Option.Let_syntax

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
  [ `Func of label * stmt list * int * int
  | `Data of label * int64 list
  ]

type dest = expr Subtype.dest

(** [seq lst] is [`Seq lst] *)
let seq lst = `Seq lst

(** [empty] is an empty sequence of statements *)
let empty : stmt = `Seq []

(** [null] is the value representing Rho's null *)
let null = `Const 0L

(** [exists_expr ~f e] is [true] iff [e] contains a node satisfying
    [f e] *)
let rec exists_expr ~f : expr -> bool = function
  | e when f e -> true
  | `Bop (_, e1, e2) -> exists_expr2 ~f e1 e2
  | `Call (_, e, es) -> exists_call ~f e es
  | `ESeq (s, e) -> exists_stmt ~f s || exists_expr ~f e
  | `Mem e -> exists_expr ~f e
  | `Const _ | `Name _ | #Temp.Virtual.t -> false

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
let rec def_expr ~init : expr -> Temp.Virtual.Set.t = function
  | `Bop (_, e1, e2) -> def_expr2 ~init e1 e2
  | `Call (_, e, es) -> def_call ~init e es
  | `ESeq (s, e) -> def_expr ~init:(def_stmt ~init s) e
  | `Mem e -> def_expr ~init e
  | `Const _ | `Name _ | #Temp.Virtual.t -> init

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
  | `Move ((`Temp _ as t), e) -> def_expr ~init:(Set.add init t) e
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
let rec use_expr ~init : expr -> Temp.Virtual.Set.t = function
  | `Bop (_, e1, e2) -> use_expr2 ~init e1 e2
  | `Call (_, e, es) -> use_call ~init e es
  | `ESeq (s, e) -> use_expr ~init:(use_stmt ~init s) e
  | #Temp.Virtual.t as t -> Set.add init t
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
  let init = Temp.Virtual.Set.empty in
  let use = use_expr ~init use in
  let def = def_expr ~init def in
  Set.are_disjoint use def

let commute e1 e2 =
  (not (has_mem e1 && has_mem e2)) && disjoint ~use:e1 ~def:e2

(** [constant e] is [true] iff [e] is a constant expression, containing
    no mem, eseq, call, or temp nodes *)
let constant e =
  let impure = function
    | `ESeq _ | `Call _ | `Mem _ | `Temp _ -> true
    | #expr -> false
  in
  not (exists_expr ~f:impure e)

(** [xi_alloc] is the xi function for allocating space *)
let xi_alloc = `Name "_xi_alloc"

(** [xi_out_of_bounds] is the xi function for an out-of-bounds array
    index *)
let xi_out_of_bounds = `Name "_xi_out_of_bounds"

let ir_pr_call name = `Call (0, name, [])

(** [alloc space] is the mir representation of a call to allocate space *)
let alloc space = `Call (1, xi_alloc, [ space ])

(** [alloc_array_const len] is a pointer to [8 * len + 8] bytes of
    memory *)
let alloc_array_const len =
  let size = Int64.(8L * succ len) in
  alloc (`Const size)

open Infix
open IrGensym

(** [index_addr p m] is [p + m * 8] as an IR expression *)
let index_addr p m = p + (m * eight)

(** [alloc_array len] is a [`Call] to [alloc] requesting [8 * len + 8]
    bytes of memory *)
let alloc_array len = alloc (index_addr eight len)

(** [index e1 e2] is the array index expression [e1\[e2\]] *)
let index e1 e2 = !(index_addr e1 e2)

(** [index_const e1 e2] is the index expression [e1\[`Const e2\]] *)
let index_const e1 e2 =
  let idx = Int64.of_int Int.(e2 * 8) in
  !(e1 + `Const idx)

(** [length e] is the length of the array beginning at pointer [e] *)
let length e = !(e - eight)

(** [check_bounds e1 e2] is jumps to an error location of [e2] is not a
    valid index for [e1], or does nothing otherwise *)
let check_bounds ~gensym e1 e2 =
  let lok, lerr = Label.fresh2 gensym in
  [
    `CJump (e2 <? length e1, lok, lerr);
    `Label lerr;
    ir_pr_call xi_out_of_bounds;
    `Label lok;
  ]

(** [encode_name s] is [s] encoded for function mangling *)
let encode_name = String.substr_replace_all ~pattern:"_" ~with_:"__"

let encode_record r =
  let len = r |> String.length |> string_of_int in
  let record_name = encode_name r in
  len ^ record_name

(** [encode_tau t] is [t] encoded for function mangling *)
let rec encode_tau = function
  | `Int -> "i"
  | `Bool -> "b"
  | `Array t -> "a" ^ encode_tau t
  | `Record r -> "r" ^ encode_record r
  | `Null -> failwith "Unexpected <null>"
  | `Bot -> failwith "Unexpected <poly>"

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

(** [encode_args args] is [args] encoded for function mangling *)
let encode_args args = args |> List.map ~f:encode_expr |> String.concat

(** [mangle_fmt] is the format used to mangle identifiers *)
let mangle_fmt = format_of_string "_I%s_%s%s"

let mangle id ~ctx =
  let arg, ret = Context.find_fn_exn ~id ctx in
  let name = id |> Entry.key |> encode_name in
  let return = encode_term ret in
  let args = arg |> Term.to_tau_list |> encode_args in
  Printf.sprintf mangle_fmt name return args

(** [num_args_returns id ctx] is a pair of the numbers of values that
    the function [id] takes in as arguments and returns. *)
let num_args_returns id ~ctx =
  let arg, ret = Context.find_fn_exn ~id ctx in
  ( arg |> Term.to_tau_list |> List.length,
    ret |> Term.to_tau_list |> List.length )

let global_defn ~gensym ~set def =
  match Entry.key def with
  | GlobalInit (id, _, _) -> Set.add set (Entry.key id)
  | _ -> set

let global_definitions ~gensym ~set defns =
  let f s defn = global_defn ~gensym ~set:s defn in
  List.fold ~f ~init:set defns

(** [translate_primitive p] is the mir representation of primitive [p] *)
let translate_primitive = function
  | `Int i -> i
  | `Bool b -> Util.Int64.of_bool b
  | `Char u -> Util.Int64.of_uchar u

(** [translate_primitive id] is the mir representation of [id] *)
let translate_id ~set id =
  let id = Entry.key id in
  if Set.mem set id then !(`Name id) else `Temp id

let translate_null ~gensym = null

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
let rec translate_expr ~gensym ~set enode =
  let ctx = Data.Expr.context @@ Entry.data enode in
  match Entry.key enode with
  | Primitive p -> `Const (translate_primitive p)
  | Id id -> translate_id ~set id
  | Array arr -> translate_array ~gensym ~set arr
  | String s -> translate_string ~gensym ~set s
  | Bop (op, e1, e2) -> translate_bop ~gensym ~set op e1 e2
  | Uop (op, e) -> translate_uop ~gensym ~set op e
  | FnCall (id, es) ->
      translate_constructor_or_call ~gensym ~set ~ctx id es
  | Length e -> translate_length ~gensym ~set e
  | Index (e1, e2) -> translate_index ~gensym ~set e1 e2
  | Null -> translate_null ~gensym
  | Field (e1, id) -> translate_field ~gensym ~set ~ctx e1 id

(** [translate_exprs es] is [es], each of which translated to IR *)
and translate_exprs ~gensym ~set es =
  List.map ~f:(translate_expr ~gensym ~set) es

(** [translate_array arr] is the mir representation of [arr] *)
and translate_array ~gensym ~set es =
  es |> translate_exprs ~gensym ~set |> array_literal ~gensym

(** [array_literal ~gensym elts] is the mir representation of a list
    [lst] *)
and array_literal ~gensym elts =
  let len = Util.List.length elts in
  let tm = Temp.fresh gensym in
  let f i e = index_const tm (Int.succ i) := e in
  let add_elts = List.mapi ~f elts in
  let alloc = tm := alloc_array_const len in
  let set_len = !tm := `Const len in
  `ESeq (`Seq (alloc :: set_len :: add_elts), tm + eight)

(** [translate_string str] is the mir representation of [str] *)
and translate_string ~gensym ~set str =
  let f c = `Const (Util.Int64.of_char c) in
  str |> String.to_list_rev |> List.rev_map ~f |> array_literal ~gensym

(** [translate_uop uop e] is the mir representation of unary operator
    expression [uop e] *)
and translate_uop ~gensym ~set uop e =
  match uop with
  | `IntNeg -> translate_int_neg ~gensym ~set e
  | `LogNeg -> e |> translate_expr ~gensym ~set |> log_neg

(** [translate_int_neg e] is the mir representation of an integer
    negation of [e] *)
and translate_int_neg ~gensym ~set e =
  match Entry.key e with
  | Primitive (`Int i) -> `Const (Int64.neg i)
  | _ -> `Bop (`Sub, zero, translate_expr ~gensym ~set e)

(** [translate_call ctx id es] is the mir representation of a function
    call with function id [id], arguments [es], and context [ctx] *)
and translate_call ~gensym ~set ~ctx id es =
  let name = mangle id ~ctx in
  let rets = num_args_returns id ~ctx |> snd in
  `Call (rets, `Name name, translate_exprs ~gensym ~set es)

and translate_constructor ~gensym ~set ~ctx id es =
  es |> translate_exprs ~gensym ~set |> record ~gensym

and record ~gensym elts =
  let len = Util.List.length elts in
  let tm = Temp.fresh gensym in
  let f i e = index_const tm (Int.succ i) := e in
  let add_elts = List.mapi ~f elts in
  let alloc = tm := alloc_array_const len in
  let set_len = !tm := `Const len in
  `ESeq (`Seq (alloc :: set_len :: add_elts), tm + eight)

and translate_constructor_or_call ~gensym ~set ~ctx id es =
  match Context.find_fn id ctx with
  | Ok _ -> (translate_call ~gensym ~set ~ctx id es :> expr)
  | Error _ -> (translate_constructor ~gensym ~set ~ctx id es :> expr)

(** [translate_bop bop e1 e2] is the mir representation of binary
    operator expression [bop e1 e2] *)
and translate_bop ~gensym ~set bop e1 e2 =
  let typ = Data.Expr.typ @@ Entry.data e1 in
  match (bop, Entry.key e1, Entry.key e2) with
  | `And, _, _ -> translate_and ~gensym ~set e1 e2
  | `Or, _, _ -> translate_or ~gensym ~set e1 e2
  | `Add, _, _ when Expr.is_array typ ->
      translate_concat ~gensym ~set e1 e2
  | #Ast.Op.binop, _, _ ->
      let e1 = translate_expr ~gensym ~set e1 in
      let e2 = translate_expr ~gensym ~set e2 in
      `Bop (Op.coerce bop, e1, e2)

(** [translate_concat e1 e2] is the mir representation of array
    concatenation [e1 @ e2] *)
and translate_concat ~gensym ~set e1 e2 : expr =
  let t1, len1, ti = Temp.fresh3 gensym in
  let t2, len2, tj = Temp.fresh3 gensym in
  let total, base, ptr = Temp.fresh3 gensym in
  let start = Temp.fresh gensym in
  let body1 = index ptr ti := index t1 ti in
  let body2 = index start tj := index t2 tj in
  `ESeq
    ( `Seq
        [
          t1 := translate_expr ~gensym ~set e1;
          t2 := translate_expr ~gensym ~set e2;
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
and translate_and ~gensym ~set e1 e2 =
  let x = Temp.fresh gensym in
  let l1, l2, lf = Label.fresh3 gensym in
  `ESeq
    ( `Seq
        [
          x := zero;
          translate_control ~gensym ~set e1 l1 lf;
          `Label l1;
          translate_control ~gensym ~set e2 l2 lf;
          `Label l2;
          x := one;
          `Label lf;
        ],
      x )

(** [translate_or e1 e2] is the mir representation of [e1 or e2] *)
and translate_or ~gensym ~set e1 e2 =
  let x = Temp.fresh gensym in
  let l1, l2, lt = Label.fresh3 gensym in
  `ESeq
    ( `Seq
        [
          x := one;
          translate_control ~gensym ~set e1 lt l1;
          `Label l1;
          translate_control ~gensym ~set e2 lt l2;
          `Label l2;
          x := zero;
          `Label lt;
        ],
      x )

(** [translate_length e] is the mir representation of a length function
    call with argument [e] *)
and translate_length ~gensym ~set e =
  length (translate_expr ~gensym ~set e)

(** [translate_index e1 e2] is the mir representation of an indexing of
    [e1] at position [e2] *)
and translate_index ~gensym ~set e1 e2 =
  let ta, ti = Temp.fresh2 gensym in
  let get_ptr = ta := translate_expr ~gensym ~set e1 in
  let get_idx = ti := translate_expr ~gensym ~set e2 in
  let bounds_check = check_bounds ~gensym ta ti in
  `ESeq (`Seq (get_ptr :: get_idx :: bounds_check), index ta ti)

and find_offset ~id count = function
  | (name, typ) :: t ->
      if String.equal name (fst id) then count
      else find_offset ~id (Int.succ count) t
  | _ -> failwith "field not found"

and translate_field ~gensym ~set ~ctx e1 id =
  let pos = snd id in
  match Ast.Decorated.Data.Expr.to_tau (snd e1) with
  | Ok (`Record record) ->
      let record = (record, pos) in
      let fields = Context.find_record_exn ~id:record ctx in
      let offset = Int64.of_int (find_offset ~id 0 fields) in
      let ta, ti = Temp.fresh2 gensym in
      let get_ptr = ta := translate_expr ~gensym ~set e1 in
      let get_idx = ti := `Const offset in
      let bounds_check = check_bounds ~gensym ta ti in
      `ESeq (`Seq (get_ptr :: get_idx :: bounds_check), index ta ti)
  | _ -> failwith "non-record field access"

(** [translate_stmt snode] is the mir representation of statement node
    [snode] *)
and translate_stmt ~gensym ~set ?lf snode =
  let ctx = Data.Stmt.context @@ Entry.data snode in
  match Entry.key snode with
  | If (e, s) -> translate_if ~gensym ~set ?lf e s
  | IfElse (e, s1, s2) -> translate_if_else ~gensym ~set ?lf e s1 s2
  | While (e, s) -> translate_while ~gensym ~set e s
  | VarDecl (ids, typ) -> translate_var_decl ~set ids typ
  | ArrayDecl (id, _, es) -> translate_array_decl ~gensym ~set id es
  | Assign (id, e) -> translate_assign ~gensym ~set id e
  | ArrAssign (e1, e2, e3) -> translate_arr_assign ~gensym ~set e1 e2 e3
  | ExprStmt (id, es) | PrCall (id, es) ->
      (translate_call ~gensym ~set ~ctx id es :> stmt)
  | VarInit (id, _, e) -> translate_assign ~gensym ~set id e
  | MultiAssign (ds, id, es) ->
      translate_multi_assign ~gensym ~set ~ctx ds id es
  | Return es -> translate_return ~gensym ~set es
  | Break -> translate_break lf
  | Block stmts -> translate_block ~gensym ~set ?lf stmts
  | FieldAssign (e1, id, e2) ->
      translate_field_assign ~gensym ~set ~ctx e1 id e2

(** [translate_if_stmt e s] is [stmts, f] where [stmts] is the first
    three IR instructions in if and if else statements, reversed, and
    [f] is the false label *)
and translate_if_stmt ~gensym ~set ?lf e s =
  let t, f = Label.fresh2 gensym in
  let s = translate_stmt ~gensym ~set ?lf s in
  ([ s; `Label t; translate_control ~gensym ~set e t f ], f)

(** [translate_if e s] is the mir representation of an if statement with
    condition [e] and body [s] *)
and translate_if ~gensym ~set ?lf e s =
  let stmts, f = translate_if_stmt ~gensym ~set ?lf e s in
  stmts |> List.cons (`Label f) |> List.rev |> seq

(** [translate_if_else e s1 s2] is the mir representation of an if-else
    statement with condition [e] and statements [s1] and [s2] *)
and translate_if_else ~gensym ~set ?lf e s1 s2 =
  let l_end = Label.fresh gensym in
  let stmts, f = translate_if_stmt ~gensym ~set ?lf e s1 in
  stmts
  |> List.cons (`Jump (`Name l_end))
  |> List.cons (`Label f)
  |> List.cons (translate_stmt ~gensym ~set ?lf s2)
  |> List.cons (`Label l_end)
  |> List.rev |> seq

(** [translate_while e s] is the mir representation of a while loop with
    condition [e] and loop body [s] *)
and translate_while ~gensym ~set e s =
  let lh, lt, lf = Label.fresh3 gensym in
  let c = translate_control ~gensym ~set e lt lf in
  let s' = translate_stmt ~gensym ~set ?lf:(Some lf) s in
  while_seq c s' lh lt lf

and translate_decl ~set typ id =
  match typ with
  | `Int | `Bool -> translate_id ~set id := `Const 0L
  | `Array _ | `Record _ -> translate_id ~set id := null
  | `Null | `Bot -> failwith "cannot declare null/bot type"

and translate_var_decl ~set ids typ =
  let translated_decls = List.map ~f:(translate_decl ~set typ) ids in
  `Seq translated_decls

(** [translate_assign_simple ta ti e] is the assignment of [e] to array
    or record [ta] at position [ti] *)
and translate_assign_simple ~gensym ~set ta ti e =
  let move = index ta ti := translate_expr ~gensym ~set e in
  `Seq [ `Seq (check_bounds ~gensym ta ti); move ]

(** [move_of_expr ~gensym e] is a pair [(mov, t)] where [t] is a fresh
    temporary and [mov] is a move statement moving [e] into [t] *)
and move_of_expr ~gensym e =
  let t = Temp.fresh gensym in
  (`Move (t, e), t)

(** [pure_exprs ~gensym  ~set es] is a pair [(moves, es')] where [moves]
    is a sequence of moves needed to move each of the impure expressions
    of [es'] into a temporary, and [es'] is the translation of [es],
    where each impure expression (that is not the first) has been moved
    into a temporary. *)
and pure_exprs ~gensym ~set es =
  let f (moves, es) e =
    let e = translate_expr ~gensym ~set e in
    if constant e then (moves, e :: es)
    else
      let s, t = move_of_expr ~gensym e in
      (s :: moves, t :: es)
  in
  let moves, es = List.fold ~f ~init:([], []) es in
  (`Seq (List.rev moves), List.rev es)

(** Same as [pure_exprs] but takes a list of optional expressions.
    [None]'s are ignored.*)
and pure_exprs_opt ~gensym ~set es =
  es |> List.filter_opt |> pure_exprs ~gensym ~set

(** [translate_array_decl_helper name es] is the mir representation of
    an array declaration for [name] with lengths [es] *)
and translate_array_decl_helper ~gensym name = function
  | [ e ] ->
      let base, ptr = Temp.fresh2 gensym in
      let tj = Temp.fresh gensym in
      let body = index ptr tj := `Const 0L in
      `Seq
        [
          base := alloc_array e;
          !base := e;
          ptr := base + eight;
          name := ptr;
          while_lt ~gensym tj e body
          (* if innermost array, assign default values *);
        ]
  | e :: es ->
      let base, ptr = Temp.fresh2 gensym in
      let ti, nested_name = Temp.fresh2 gensym in
      let nested =
        `Seq
          [
            translate_array_decl_helper ~gensym nested_name es;
            index ptr ti := nested_name;
          ]
      in
      `Seq
        [
          base := alloc_array e;
          !base := e;
          ptr := base + eight;
          name := ptr;
          while_lt ~gensym ti e nested;
        ]
  | [] -> empty

(** [translate_array_decl id es] is the mir representation of an array
    declaration for variable [id] with lengths [es] *)
and translate_array_decl ~gensym ~set id es =
  let name = `Temp (Entry.key id) in
  let moves, es = pure_exprs_opt ~gensym ~set es in
  let decl = translate_array_decl_helper ~gensym name es in
  `Seq [ moves; decl ]

(** [translate_assign id e] is the mir representation of an assignment
    of [e] to [id] *)
and translate_assign ~gensym ~set id e =
  translate_id ~set id := translate_expr ~gensym ~set e

(** [translate_arr_assign e1 e2 e3] is the mir representation of a array
    assignment statement with array reference [e1], position [e2], and
    value [e3] *)
and translate_arr_assign ~gensym ~set e1 e2 e3 =
  let ta, ti = Temp.fresh2 gensym in
  `Seq
    [
      ta := translate_expr ~gensym ~set e1;
      ti := translate_expr ~gensym ~set e2;
      translate_assign_simple ~gensym ~set ta ti e3;
    ]

and translate_field_assign ~gensym ~set ~ctx e1 id e2 =
  let ta, ti = Temp.fresh2 gensym in
  let pos = snd id in
  let offset =
    match Ast.Decorated.Data.Expr.to_tau (snd e1) with
    | Ok (`Record record) ->
        let record = (record, pos) in
        let fields = Context.find_record_exn ~id:record ctx in
        Int64.of_int (find_offset ~id 0 fields)
    | _ -> failwith "non-record field access"
  in
  `Seq
    [
      ta := translate_expr ~gensym ~set e1;
      ti := `Const offset;
      translate_assign_simple ~gensym ~set ta ti e2;
    ]

(** [translate_multi_assign ctx ds id es] is the mir representation of a
    multi-assignment with declarations [ds], and function [id], and
    arguments [es] *)
and translate_multi_assign ~gensym ~set ~ctx ds id es =
  let call = (translate_call ~gensym ~set ~ctx id es :> stmt) in
  let f i d =
    let%map id, _ = d in
    translate_id ~set id := `Rv (Int.succ i)
  in
  `Seq (call :: List.filter_mapi ~f ds)

(** [translate_return es] is the mir representation of a return
    statement with expressions [es] *)
and translate_return ~gensym ~set es =
  `Return (translate_exprs ~gensym ~set es)

and translate_break = function
  | Some lf -> `Jump (`Name lf)
  | None -> failwith "break not in while loop, should not typecheck"

(** [translate_block stmts] is the mir representation of a statement
    block with statements [stmts] *)
and translate_block ~gensym ~set ?lf stmts =
  stmts |> List.map ~f:(translate_stmt ~gensym ~set ?lf) |> seq

(** [translate_control enode t f] translates booleans to control flow *)
and translate_control ~gensym ~set enode t f =
  match Entry.key enode with
  | Primitive (`Bool b) -> `Jump (`Name (if b then t else f))
  | Uop (`LogNeg, e) -> translate_control ~gensym ~set e f t
  | Bop (`And, e1, e2) ->
      translate_short_circuit ~short:false ~gensym ~set e1 e2 t f
  | Bop (`Or, e1, e2) ->
      translate_short_circuit ~short:true ~gensym ~set e1 e2 t f
  | _ -> `CJump (translate_expr ~gensym ~set enode, t, f)

(** [translate_short_cricuit ~short ~gensym ~set e1 e2 t f] is s
    statement computing [`CJump (e1 | e2, t, f)] if [short] is [true],
    and the translation of [`CJump (e1 & e2, t, f)] otherwise *)
and translate_short_circuit ~short ~gensym ~set e1 e2 t f =
  let label = Label.fresh gensym in
  let t', f' = if short then (t, label) else (label, f) in
  `Seq
    [
      translate_control ~gensym ~set e1 t' f';
      `Label label;
      translate_control ~gensym ~set e2 t f;
    ]

(** [returns_unit ctx id] is true if the function associated with [id]
    in [ctx] has a return of type Unit, false otherwise *)
let returns_unit ~ctx id =
  match Context.find_fn_exn ~id ctx with _, `Unit -> true | _ -> false

(** [translate_fn_defn sign] is the mir representation of a function
    definition with signature [sign] *)
let translate_fn_defn ~gensym ~set ~ctx signature block =
  let id = Sig.name signature in
  let name = mangle id ~ctx in
  let f i (id, _) = `Temp (Entry.key id) := `Arg (Int.succ i) in
  let moves = List.mapi ~f @@ Sig.params signature in
  let block = translate_block ~gensym ~set block in
  let ret =
    if returns_unit ~ctx id then [ translate_return ~gensym ~set [] ]
    else []
  in
  let num_args, num_rets = num_args_returns ~ctx id in
  `Func (name, `Seq moves :: block :: ret, num_args, num_rets)

(** [translate_global_init id typ p] is the mir representation of a
    global initialization of [id] with type [typ] and primitive [p] as
    its value *)
let translate_global_init id p =
  `Data (Entry.key id, [ translate_primitive p ])

(** [translate_defn def] is the mir representation of source toplevel
    definition [def] *)
let translate_defn ~gensym ~set def =
  match Entry.key def with
  | FnDefn (signature, block) ->
      let ctx = Data.Toplevel.context @@ Entry.data def in
      (* TODO : fix this formatting *)
      Some
        (translate_fn_defn ~gensym ~set ~ctx signature block
          :> toplevel)
  | GlobalDecl _ -> None
  | GlobalInit (id, _, p) -> Some (translate_global_init id p)
  | RecordDefn (id, decls) -> None

let data_of_string id s =
  let open Util.Int64 in
  let lst = s |> String.to_list_rev |> List.rev_map ~f:of_char in
  `Data (id, Util.List.length lst :: lst)

let translate ~gensym src =
  let defs = Source.defs src in
  let set = global_definitions ~gensym ~set:String.Set.empty defs in
  List.filter_map ~f:(translate_defn ~gensym ~set) defs
