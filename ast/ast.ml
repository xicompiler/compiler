include Factory.Make (Node.Ident) (Node.Ident)
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

let type_check_primitive ctx p =
  Decorated.Ex.make p ~ctx ~typ:(type_of_primitive p)

(** [type_check expr] is [Ok expr'] where [expr'] is [expr] decorated or
    [Error type_error] if where [type_error] describes a semantic error
    of [expr] *)
let type_check_expr ctx = function
  | Primitive p -> type_check_primitive ctx p
  | _ -> failwith "unimplemented"