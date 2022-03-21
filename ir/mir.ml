open Subtype
open Core
open Int64
module PosNode = Node.Position
module DecNode = Context.Node.Decorated
open Ast.Op
open Ast.Decorated
open Expr
open Stmt
open Toplevel
open Primitive
open Type

type expr =
  [ expr Subtype.expr
  | `Call of expr * expr list
  | `ESeq of stmt * expr
  ]

and stmt =
  [ expr Subtype.stmt
  | `CallStmt of expr * expr list
  ]

let one = `Const one
let zero = `Const zero
let eight = `Const 8L
let length lst = lst |> List.length |> Int64.of_int
let to_addr n = (8L * n) + 8L

let make_fresh pre counter =
  Int.incr counter;
  pre ^ Int.to_string !counter

let label_gen = GenSym.create "l%d"
let temp_gen = GenSym.create "x%d"

(** [make_fresh_label ()] makes a fresh (unused elsewhere) label with
    prefix "l" and integer based on [label_counter] *)
let make_fresh_label () = GenSym.generate label_gen

(** [make_fresh_temp ()] makes a fresh (unused elsewhere) temp name with
    prefix "x" and integer based on [temp_counter] *)
let make_fresh_temp () = GenSym.generate temp_gen

let rec encode_tau = function
  | `Int -> "i"
  | `Bool -> "b"
  | `Array t -> "a" ^ encode_tau t
  | `Poly -> failwith "Unexpected <poly>"

let encode_expr = function
  | `Tuple ts ->
      let len = ts |> List.length |> string_of_int in
      let types = ts |> List.map ~f:encode_tau |> String.concat in
      "t" ^ len ^ types
  | #Tau.t as t -> encode_tau t

let encode_term = function
  | `Unit -> "p"
  | #Expr.t as t -> encode_expr t

let encode_name = String.substr_replace_all ~pattern:"_" ~with_:"__"
let encode_args args = args |> List.map ~f:encode_expr |> String.concat

let mangle id ~ctx =
  let arg, ret = Context.find_fn_exn ~id ctx in
  let name = id |> PosNode.get |> encode_name in
  let return = encode_term ret in
  let args = arg |> tau_list_of_term |> encode_args in
  Printf.sprintf "_I%s_%s%s" name return args

let translate_primitive p =
  match p with
  | `Int i -> `Const i
  | `Bool b -> if b then one else zero
  | `Char c -> `Const (c |> Uchar.to_scalar |> Int64.of_int)

let translate_id id = `Temp (PosNode.get id)

let rec translate_expr enode =
  let ctx = DecNode.Expr.context enode in
  match DecNode.Expr.get enode with
  | Primitive p -> translate_primitive p
  | Id id -> translate_id id
  | Array arr -> translate_arr arr
  | String s -> translate_string s
  | Bop (op, e1, e2) -> translate_bop op e1 e2
  | Uop (op, e) -> translate_uop op e
  | FnCall (id, es) -> translate_fncall ~ctx id es
  | Length node -> translate_length node
  | Index (e1, e2) -> translate_index e1 e2

and translate_arr arr =
  let expr_lst = List.map ~f:translate_expr arr in
  translate_arr_lst expr_lst

and translate_arr_lst lst =
  let n = length lst in
  let tm = make_fresh_temp () in
  let f acc e =
    let index = acc |> length |> to_addr in
    `Move (`Mem (`Bop (`Plus, `Name tm, `Const index)), e) :: acc
  in
  let add_elts = List.rev (List.fold ~f ~init:[] lst) in
  `ESeq
    ( `Seq
        (`Move
           (`Temp tm, `Call (`Name "_xi_alloc", [ `Const (to_addr n) ]))
        :: `Move (`Mem (`Temp tm), `Const n)
        :: add_elts),
      `Bop (`Plus, `Temp tm, eight) )

and translate_string str =
  let expr_lst =
    List.map
      ~f:(fun s -> `Const (s |> int_of_char |> Int64.of_int))
      (String.to_list str)
  in
  translate_arr_lst expr_lst

and translate_uop uop e =
  let ir = translate_expr e in
  match uop with
  | `IntNeg -> `Bop (`Plus, `Bop (`Xor, ir, one), one)
  | `LogNeg -> `Bop (`Xor, ir, one)

and translate_bop bop e1 e2 =
  match bop with
  | `And -> translate_and e1 e2
  | `Or -> translate_or e1 e2
  | #Ast.Op.binop ->
      `Bop (Op.coerce bop, translate_expr e1, translate_expr e2)

and translate_and e1 e2 =
  let x = make_fresh_temp () in
  let l1 = make_fresh_label () in
  let l2 = make_fresh_label () in
  let lf = make_fresh_label () in
  `ESeq
    ( `Seq
        [
          `Move (`Temp x, zero);
          translate_control e1 l1 lf;
          `Label l1;
          translate_control e2 l2 lf;
          `Label l2;
          `Move (`Temp x, one);
          `Label lf;
        ],
      `Temp x )

and translate_or e1 e2 =
  let x = make_fresh_temp () in
  let l1 = make_fresh_label () in
  let l2 = make_fresh_label () in
  let lt = make_fresh_label () in
  `ESeq
    ( `Seq
        [
          `Move (`Temp x, one);
          translate_control e1 lt l1;
          `Label l1;
          translate_control e2 lt l2;
          `Label l2;
          `Move (`Temp x, zero);
          `Label lt;
        ],
      `Temp x )

and translate_fncall ~ctx id es =
  let name = mangle id ~ctx in
  let expr_lst = List.map ~f:translate_expr es in
  `Call (`Name name, expr_lst)

and translate_length node =
  let ir = translate_expr node in
  `Mem (`Bop (`Minus, ir, eight))

and translate_index e1 e2 =
  let ta = make_fresh_temp () in
  let ti = make_fresh_temp () in
  let lok = make_fresh_label () in
  let lerr = make_fresh_label () in
  `ESeq
    ( `Seq
        [
          `Move (`Temp ta, translate_expr e1);
          `Move (`Temp ti, translate_expr e2);
          `CJump
            ( `Bop
                (`ULt, `Temp ti, `Mem (`Bop (`Minus, `Name ta, eight))),
              lok,
              lerr );
          `Label lerr;
          `Call (`Name "_xi_out_of_bounds", []);
          `Label lok;
        ],
      `Mem (`Bop (`Plus, ta, `Bop (`Mult, ti, eight))) )

and translate_stmt snode =
  let ctx = DecNode.Stmt.context snode in
  match DecNode.Stmt.get snode with
  | If (e, s) -> Some (translate_if e s)
  | IfElse (e, s1, s2) -> Some (translate_if_else e s1 s2)
  | While (e, s) -> Some (translate_while e s)
  | VarDecl _ -> None
  | ArrayDecl _ -> None
  | Assign (id, e) -> Some (translate_assign id e)
  | ArrAssign (e1, e2, e3) -> Some (translate_arr_assign e1 e2 e3)
  | ExprStmt (id, es) -> Some (translate_expr_stmt ~ctx id es)
  | VarInit (id, _, e) -> Some (translate_assign id e)
  | MultiAssign (ds, id, es) ->
      Some (translate_multi_assign ~ctx ds id es)
  | PrCall (id, es) -> Some (translate_prcall ~ctx id es)
  | Return es -> Some (translate_return es)
  | Block stmts -> Some (translate_block stmts)

and translate_if e s =
  let t_label = make_fresh_label () in
  let f_label = make_fresh_label () in
  `Seq
    (List.filter_opt
       [
         Some (translate_control e t_label f_label);
         Some (`Label t_label);
         translate_stmt s;
         Some (`Label f_label);
       ])

and translate_if_else e s1 s2 =
  let t_label = make_fresh_label () in
  let f_label = make_fresh_label () in
  let end_label = make_fresh_label () in
  `Seq
    (List.filter_opt
       [
         Some (translate_control e t_label f_label);
         Some (`Label t_label);
         translate_stmt s1;
         Some (`Jump (`Name end_label));
         Some (`Label f_label);
         translate_stmt s2;
         Some (`Label end_label);
       ])

and translate_while e s =
  let h_label = make_fresh_label () in
  let t_label = make_fresh_label () in
  let f_label = make_fresh_label () in
  `Seq
    (List.filter_opt
       [
         Some (`Label h_label);
         Some (translate_control e t_label f_label);
         Some (`Label t_label);
         translate_stmt s;
         Some (`Jump (`Name h_label));
         Some (`Label f_label);
       ])

and translate_assign id e =
  let expr = translate_expr e in
  `Move (`Temp (PosNode.get id), expr)

and translate_arr_assign e1 e2 e3 =
  let ta = make_fresh_temp () in
  let ti = make_fresh_temp () in
  let lok = make_fresh_label () in
  let lerr = make_fresh_label () in
  `Seq
    [
      `Move (`Temp ta, translate_expr e1);
      `Move (`Temp ti, translate_expr e2);
      `CJump
        ( `Bop (`ULt, `Temp ti, `Mem (`Bop (`Minus, `Name ta, eight))),
          lok,
          lerr );
      `Label lerr;
      `Call (`Name "_xi_out_of_bounds", []);
      `Label lok;
      `Move
        ( `Mem (`Bop (`Plus, `Name ta, `Bop (`Mul, `Name ti, eight))),
          translate_expr e3 );
    ]

and translate_expr_stmt ~ctx id es =
  let name = mangle id ~ctx in
  let expr_lst = List.map ~f:translate_expr es in
  `CallStmt (`Name name, expr_lst)

and translate_multi_assign ~ctx ds id es =
  let name = mangle id ~ctx in
  let expr_lst = List.map ~f:translate_expr es in
  let call = `CallStmt (`Name name, expr_lst) in
  let f (rv, acc) = function
    | None -> (Int.succ rv, acc)
    | Some (v, _) ->
        let t = "_RV" ^ Int.to_string rv in
        let lst = `Move (`Temp (PosNode.get v), `Temp t) :: acc in
        (Int.succ rv, lst)
  in
  let assign = ds |> List.fold ~f ~init:(1, []) |> snd |> List.rev in
  `Seq (call :: assign)

and translate_prcall ~ctx id es =
  let name = mangle id ~ctx in
  let expr_lst = List.map ~f:translate_expr es in
  `CallStmt (`Name name, expr_lst)

and translate_return es = `Return (List.map ~f:translate_expr es)

and translate_block stmts =
  `Seq (List.filter_map ~f:translate_stmt stmts)

and translate_fn_defn ({ id } : Ast.signature) block =
  `Seq [ `Label (PosNode.get id); translate_block block ]

and translate_global_init id typ prim =
  `Move (`Temp (PosNode.get id), prim)

and translate_defn def =
  match DecNode.Toplevel.get def with
  | FnDefn (signature, block) ->
      Some (translate_fn_defn signature block)
  | GlobalDecl _ -> None
  | GlobalInit (id, typ, prim) ->
      Some (translate_global_init id typ prim)

and translate_source { uses; definitions } =
  `Seq (List.filter_map ~f:translate_defn definitions)

and translate_toplevel tnode =
  match tnode with
  | Source src -> translate_source src
  | Intf intf -> failwith "unimplemented"

(** [translate_control enode t f] translates booleans to control flow *)
and translate_control enode t f =
  match DecNode.Expr.get enode with
  | Primitive (`Bool true) -> `Jump (`Name t)
  | Primitive (`Bool false) -> `Jump (`Name f)
  | Uop (`LogNeg, e) -> translate_control e f t
  | Bop (`And, e1, e2) ->
      let label = make_fresh_label () in
      `Seq
        [
          translate_control e1 label f;
          `Label label;
          translate_control e2 t f;
        ]
  | Bop (`Or, e1, e2) ->
      let label = make_fresh_label () in
      `Seq
        [
          translate_control e1 t label;
          `Label label;
          translate_control e2 t f;
        ]
  | _ ->
      let expr = translate_expr enode in
      `CJump (expr, t, f)
