open Core
open Result.Monad_infix
open Result.Let_syntax

include Factory.Make (Node.Pos) (Node.Pos)
module Tau = Tau

open Expr
module Context = Decorated.Context

let type_check ast = failwith "unimplemented"

(** [type_of_primitive p] is [`Int] if [p] is [Int] or [Char] and
    [`Bool] if [p] is [Bool] *)
let type_of_primitive = function
  | Int _
  | Char _ ->
      `Int
  | Bool _ -> `Bool

let type_of_id ctx i = 
  Result.of_option (Context.find ctx i) ~error:Decorated.Type.Unbound
  
(** [type_check_primitive ctx p] is [Ok expr] where [expr] is a
    decorated expression node for [p]. *)
let type_check_primitive ctx p =
  Ok (Decorated.Ex.make p ~ctx ~typ:(type_of_primitive p))

(** [type_check_id ctx i] is [Ok expr] where [expr] is a
    decorated expression node for [i] if [i] is in [ctx], or
    [Error err] otherwise. *)
let type_check_id ctx i = 
  type_of_id ctx i >>| function
    | Var tau -> Decorated.Ex.make i ~ctx ~typ:tau
    | _ -> Error (Decorated.Type.ExpectedVar i)

let type_check_array ctx arr = failwith "unimplemented"

let type_check_string ctx str = failwith "unimplemented"

let type_check_bop ctx op n1 n2 = match op with
  | Mult
  | HighMult
  | Div
  | Mod
  | Minus
  | Lt
  | Leq
  | Geq
  | Gt
  | Eq
  | Neq -> begin match (type_check_expr n1, type_check_expr n2) with
    | _ -> failwith "unimplemented" end
  | And
  | Or
  | Plus
  | _ -> failwith "unimplemented"

let type_check_uop ctx op n = failwith "unimplemented"

let type_check_fncall ctx call = failwith "unimplemented"

let type_check_length ctx node = failwith "unimplemented"

let type_check_index ctx i = failwith "unimplemented"

let type_check_if ctx expr n1 n2 opt = failwith "unimplemented"

let type_check_while ctx expr node = failwith "unimplemented"

let type_check_decl ctx decl = failwith "unimplemented"

let type_check_init ctx decl = failwith "unimplemented"

(** [type_check_expr ctx expr] is [Ok expr'] where [expr'] is [expr] decorated or
    [Error type_error] where [type_error] describes a semantic error
    of [expr] *)
let type_check_expr ctx = function
  | Primitive p -> type_check_primitive ctx p
  | Id i -> type_check_id ctx i
  | Array arr -> type_check_array ctx arr
  | String s -> type_check_string ctx str
  | Bop (op, n1, n2) -> type_check_bop ctx op n1 n2
  | Uop (op, n) -> type_check_uop ctx op n
  | FnCall call -> type_check_fncall ctx call
  | Length node -> type_check_length ctx node
  | Index i -> type_check_index ctx i

let type_check_statement ctx = function
  | If (expr_node, n1, n2) -> failwith "unimplemented"
  | While (expr_node, node) -> failwith "unimplemented"
  | VarDecl decl -> failwith "unimplemented"
  | ArrayDecl (id, typ, nodes) -> failwith "unimplemented"
  | Assign (id, node) -> failwith "unimplemented"
  | ArrAssign (n1, n2, n3) -> failwith "unimplemented"
  | ExprStmt call -> failwith "unimplemented"
  | VarInit (id, t, node) -> failwith "unimplemented"
  | MultiAssign (list, id, nodes) -> failwith "unimplemented"
  | PrCall call -> failwith "unimplemented"
  | Return nodes -> failwith "unimplemented"
  | Block block -> failwith "unimplemented"
