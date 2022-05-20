open Core
open Option.Let_syntax
open Option.Monad_infix
open Util.Fn

module VN = struct
  type t =
    [ `Bop of Op.t * int * int
    | `Temp of string
    | `Const of int64
    ]
  [@@deriving hash, compare, sexp]

  type comm =
    [ `Add
    | `Mul
    | `HMul
    | `And
    | `Or
    | `Eq
    | `Neq
    ]

  let canonical : t -> t = function
    | `Bop ((#comm as op), i1, i2) -> `Bop (op, min i1 i2, max i1 i2)
    | e -> e

  let hash e = hash (canonical e)
  let compare e1 e2 = compare (canonical e1) (canonical e2)
end

module Table = Hashtbl.Make (VN)

let table = Table.create ()

let find_vn map e =
  let default () = Hashtbl.length map in
  Hashtbl.find_or_add map e ~default

let rec value_number map : Lir.expr -> int option = function
  | (`Const _ | `Temp _) as base -> Some (find_vn map base)
  | `Bop (op, e1, e2) -> number_bop op e1 e2 map
  | `Name _ | `Mem _ | #Temp.Virtual.t -> None

and number_bop op e1 e2 map =
  let%bind n1 = value_number map e1 in
  let%map n2 = value_number map e2 in
  find_vn map (`Bop (op, n1, n2))

let find_names ~name i =
  Hashtbl.find_or_add name i ~default:String.Table.create

let add_name ~name t i =
  let names = find_names ~name i in
  Hashtbl.set names ~key:t ~data:()

let find_name ~name i =
  let names = find_names ~name i in
  Hashtbl.choose names >>| fst

let remove_prev ~vn ~name t =
  let num = Hashtbl.find vn (`Temp t) in
  Option.iter num ~f:(fun i ->
      let names = find_names ~name i in
      Hashtbl.remove names t)

let update ~vn ~name t e =
  remove_prev ~vn ~name t;
  Option.iter (value_number vn e) ~f:(add_name ~name t)

let rec subst ~vn ~name (e : Lir.expr) : Lir.expr =
  match value_number vn e >>= find_name ~name with
  | Some t -> `Temp t
  | None -> subst_expr ~vn ~name e

and subst_expr ~vn ~name =
  let subst = subst ~vn ~name in
  function
  | (`Const _ | #Temp.Virtual.t | `Name _) as base -> base
  | `Bop (op, e1, e2) -> `Bop (op, subst e1, subst e2)
  | `Mem e -> `Mem (subst e)

let subst_stmt ~vn ~name : Lir.stmt -> Lir.stmt =
  let subst = subst ~vn ~name in
  let subst_list = List.map ~f:subst in
  function
  | `Move ((`Temp t as dst), e) ->
      let stmt = `Move (dst, subst e) in
      update ~vn ~name t e;
      stmt
  | `Move (`Mem addr, e) -> `Move (`Mem (subst addr), subst e)
  | `CJump (e, t, f) -> `CJump (subst e, t, f)
  | `Call (m, e, es) -> `Call (m, subst e, subst_list es)
  | `Jump e -> `Jump (subst e)
  | `Return es -> `Return (subst_list es)
  | `Label _ as l -> l

let number_block =
  let vn = Table.create () in
  let name = Int.Table.create () in
  List.map ~f:(subst_stmt ~vn ~name)

let number_fn =
  BasicBlock.group >> List.map ~f:number_block >> List.concat

let number_toplevel : Lir.toplevel -> Lir.toplevel = function
  | `Data _ as d -> d
  | `Func (l, b, a, r) -> `Func (l, number_fn b, a, r)

let number = List.map ~f:number_toplevel
