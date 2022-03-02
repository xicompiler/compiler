open Core
open Result.Monad_infix
include Factory.Make (Node.Pos) (Node.Pos)
open Expr
module Context = Decorated.Context

type error = 
  | UnboundError

let type_check ast = failwith "unimplemented"

(** [type_of_primitive p] is [`Int] if [p] is [Int] or [Char] and
    [`Bool] if [p] is [Bool] *)
let type_of_primitive = function
  | Int _
  | Char _ ->
      `Int
  | Bool _ -> `Bool

let type_of_id ctx i =
  match Context.find ctx i with
  | Some t -> Ok t
  | None -> Error UnboundError
  
(** [type_check_primitive ctx p] is [Ok expr] where [expr] is a
    decorated expression node for [p]. *)
let type_check_primitive ctx p =
  Ok (Decorated.Ex.make p ~ctx ~typ:(type_of_primitive p))

(** [type_check_id ctx i] is [Ok expr] where [expr] is a
    decorated expression node for [i] if [i] is in [ctx], or
    [Error err] otherwise. *)
let type_check_id ctx i = 
  type_of_id ctx i >>| fun typ -> Decorated.Ex.make i ~ctx ~typ

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
  | Neq -> begin match (type_check n1, type_check n2) with
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
  | If (expr_node, n1, n2, opt) -> failwith "unimplemented"
  | While (expr_node, node) -> failwith "unimplemented"
  | Decl decl -> failwith "unimplemented"
  | Init init -> failwith "unimplemented"
  | Assign (target, node) -> failwith "unimplemented"
  | MultiInit (target, list, call) -> failwith "unimplemented"
  | PrCall call -> failwith "unimplemented"
  | Return list -> failwith "unimplemented"
  | Block block -> failwith "unimplemented"
