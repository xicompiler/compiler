open Core

module Expr = struct
  type t =
    | Bop of Op.t * int * int
    | Temp of string
    | Const of int64
  [@@deriving hash, compare, sexp]
end

module Table = Hashtbl.Make (Expr)

let table = Table.create ()

let find_expr e map =
  let result =
    Hashtbl.find_or_add map e ~default:(fun () -> Hashtbl.length map)
  in
  Some result

let rec number_expr map = function
  | `Const i -> find_expr (Expr.Const i) map
  | `Bop (op, e1, e2) -> number_bop op e1 e2 map
  | `Temp t -> find_expr (Expr.Temp t) map
  | `Name _ | `Mem _ | #Temp.Virtual.t -> None

and number_bop op e1 e2 map =
  match (number_expr map e1, number_expr map e2) with
  | Some n1, Some n2 -> begin
      match op with
      | `Add | `Mul | `HMul | `And | `Or | `Eq | `Neq ->
          find_expr (Expr.Bop (op, Int.min n1 n2, Int.max n1 n2)) map
      | _ -> find_expr (Expr.Bop (op, n1, n2)) map
    end
  | _ -> None

let number_stmt map (stmt : Reorder.stmt) =
  match stmt with
  | `Move (`Temp t, e) -> begin
      match number_expr map e with
      | Some n ->
          Hashtbl.set map ~key:(Expr.Temp t) ~data:n;
          map
      | None -> map
    end
  | `Label _ | `CJump _ | `Call _ | `Jump _ | `Move _ | `Return _ -> map

let number_stmts stmts = List.fold stmts ~init:table ~f:number_stmt
