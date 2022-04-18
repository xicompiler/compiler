open Core
open Option.Let_syntax
open Frontend
open Subtype

(** [const_of_base b] is [`Const r] if [b] is [Some r] and [None]
    otherwise *)
let const_of_base b =
  let%map r = b in
  `Const r

(** [const_fold_expr expr] is LIR expression [expr] constant folded *)
let rec const_fold_expr : Lir.expr -> Lir.expr = function
  | (`Const _ | `Name _) as e -> e
  | `Bop (op, e1, e2) -> const_fold_bop op e1 e2
  | `Mem e -> `Mem (const_fold_expr e)
  | #Temp.Virtual.t as e -> e

(** [const_fold_dest] is [dest] constant folded *)
and const_fold_dest : Lir.expr Subtype.dest -> Lir.expr Subtype.dest =
  function
  | `Mem e -> `Mem (const_fold_expr e)
  | `Temp _ as t -> t

(** [const_fold_bop_opt bop e1 e2] is the IR node [e1 bop e2] where all
    constant expressions have been folded *)
and const_fold_bop_opt bop e1 e2 =
  match (bop, e1, e2) with
  | op, `Const i1, `Const i2 -> const_of_base (Op.eval op i1 i2)
  | `Xor, `Bop (`Xor, e, `Const 1L), `Const 1L ->
      Some (const_fold_expr e)
  | _ -> None

(** [const_fold_bop op e1 e2] is operation [e1 op e2] constant folded *)
and const_fold_bop op e1 e2 =
  let e1 = const_fold_expr e1 in
  let e2 = const_fold_expr e2 in
  let default () = `Bop (op, e1, e2) in
  Util.Option.Lazy.value ~default (const_fold_bop_opt op e1 e2)

(** [const_fold_stmt stmt] is LIR statement [stmt] constant folded *)
let const_fold_stmt : Reorder.stmt -> Reorder.stmt = function
  | `Label l as s -> s
  | `CJump (e, l) -> `CJump (const_fold_expr e, l)
  | `Call (i, e, es) ->
      let e = const_fold_expr e in
      let es = List.map es ~f:const_fold_expr in
      `Call (i, e, es)
  | `Jump e -> `Jump (const_fold_expr e)
  | `Move (dest, e) ->
      let dest = const_fold_dest dest in
      let e = const_fold_expr e in
      `Move (dest, e)
  | `Return es -> `Return (List.map es ~f:const_fold_expr)

(** [const_fold_toplevel tl] is LIR toplevel [tl] constant folded *)
let const_fold_toplevel : Reorder.toplevel -> Reorder.toplevel =
  function
  | `Data _ as d -> d
  | `Func (l, b, a, r) -> `Func (l, List.map ~f:const_fold_stmt b, a, r)

let const_fold = List.map ~f:const_fold_toplevel

(** [sexp_of_const i] is the sexp representation of [`Const i] *)
let sexp_of_const i = Sexp.List [ Sexp.Atom "CONST"; Int64.sexp_of_t i ]

(** [sexp_of_name l] is the sexp representation of [`Name l] *)
let sexp_of_name l = Sexp.List [ Sexp.Atom "NAME"; Sexp.Atom l ]

(** [sexp_of_expr expr] is the sexp representation of [expr] *)
let rec sexp_of_expr = function
  | `Const i -> sexp_of_const i
  | `Bop (op, e1, e2) -> sexp_of_bop op e1 e2
  | `Name l -> sexp_of_name l
  | `Mem e -> sexp_of_mem e
  | #Temp.Virtual.t as e -> sexp_of_temp (Temp.Virtual.to_string e)

(** [sexp_of_bop] is the sexp representation of [`Bop op e1 e2] *)
and sexp_of_bop op e1 e2 =
  Sexp.List
    [ Sexp.Atom (Op.to_string op); sexp_of_expr e1; sexp_of_expr e2 ]

(** [sexp_of_dest d] is the sexp representation of [dest] *)
and sexp_of_dest = function
  | `Mem e -> sexp_of_mem e
  | `Temp t -> sexp_of_temp t

(** [sexp_of_mem e] is the sexp representation of [`Mem e] *)
and sexp_of_mem e = Sexp.List [ Sexp.Atom "MEM"; sexp_of_expr e ]

(** [sexp_of_temp t] is the sexp representation of [`Temp t] *)
and sexp_of_temp t = Sexp.List [ Sexp.Atom "TEMP"; Sexp.Atom t ]

(** [sexp_of_move d e] is the sexp representation of [`Move d e] *)
let sexp_of_move d e =
  Sexp.List [ Sexp.Atom "MOVE"; sexp_of_dest d; sexp_of_expr e ]

(** [sexp_of_jump e] is the sexp representation of [`Jump e] *)
let sexp_of_jump e = Sexp.List [ Sexp.Atom "JUMP"; sexp_of_expr e ]

(** [sexp_of_call_stmt i e es] is the sexp representation of
    [`Call i e es] *)
let sexp_of_call_stmt i e es =
  Sexp.List
    (Sexp.Atom "CALL_STMT"
    :: Sexp.Atom (Int.to_string i)
    :: sexp_of_expr e
    :: List.map ~f:sexp_of_expr es)

(** [sexp_of_jump e] is the sexp representation of [`CJump e l] *)
and sexp_of_cjump e l =
  Sexp.List [ Sexp.Atom "CJUMP"; sexp_of_expr e; Sexp.Atom l ]

(** [sexp_of_label s] is the sexp representation of [`Label s] *)
let sexp_of_label s = Sexp.List [ Sexp.Atom "LABEL"; Sexp.Atom s ]

(** [sexp_of_return es] is the sexp representation of [`Return es] *)
let sexp_of_return es =
  Sexp.List (Sexp.Atom "RETURN" :: List.map ~f:sexp_of_expr es)

(** [sexp_of_stmt stmt] is the sexp representation of [stmt] *)
let sexp_of_stmt = function
  | `Label l -> sexp_of_label l
  | `Move (d, e) -> sexp_of_move d e
  | `Jump e -> sexp_of_jump e
  | `Return es -> sexp_of_return es
  | `Call (i, e, es) -> sexp_of_call_stmt i e es
  | `CJump (e, l) -> sexp_of_cjump e l

(** [sexp_of_data l i] is the sexp representation of [`Data (l, i)] *)
let sexp_of_data l i =
  Sexp.List
    [ Sexp.Atom "DATA"; Sexp.Atom l; List.sexp_of_t Int64.sexp_of_t i ]

(** [sexp_of_function l b] is the sexp representation of [`Func l b] *)
let sexp_of_function l b =
  Sexp.List
    [
      Sexp.Atom "FUNC";
      Sexp.Atom l;
      Sexp.List (Sexp.Atom "SEQ" :: List.map ~f:sexp_of_stmt b);
    ]

(** [sexp_of_toplevel tl] is the sexp representation of [tl] *)
let sexp_of_toplevel = function
  | `Data (l, i) -> sexp_of_data l i
  | `Func (l, b, _, _) -> sexp_of_function l b

let sexp_of_t ~compunit (top : Reorder.t) : Sexp.t =
  Sexp.List
    (Sexp.Atom "COMPUNIT" :: Sexp.Atom compunit
    :: List.map ~f:sexp_of_toplevel top)

let translate ~optimize ?(gensym = IrGensym.create ()) ast =
  let stmts =
    ast |> Mir.translate ~gensym |> Lir.lower ~gensym
    |> Reorder.reorder ~gensym:(IrGensym.Label.generator gensym)
  in
  if optimize then const_fold stmts else stmts

module Output = struct
  (** [print_source ~out ~compunit ~optimize source] prints [source] to
      [out] as an s-expression *)
  let print_source ~out ~compunit ~optimize source =
    source |> translate ~optimize |> sexp_of_t ~compunit
    |> Sexp.to_string_hum |> Util.File.println ~out

  let file_to_file ?cache ~src ~out ~deps ~optimize () =
    let compunit = Util.File.base src in
    let f =
      Ast.iter_source ~f:(print_source ~out ~compunit ~optimize)
    in
    Check.Diagnostic.iter_file ?cache ~src ~out ~deps ~f ()
end

include Subtype
module Mir = Mir
module Lir = Lir
module Reorder = Reorder
module Subtype = Subtype
module Op = Op
module Gensym = IrGensym
module Temp = Temp
