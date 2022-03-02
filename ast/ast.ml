open Core
open Result.Monad_infix
open Result.Let_syntax
include Factory.Make (Node.Pos) (Node.Pos)
open Expr
open Stmt
open Type
open Primitive

let type_check ast = failwith "unimplemented"

(** [type_of_primitive p] is [`Int] if [p] is [Int] or [Char] and
    [`Bool] if [p] is [Bool] *)
let type_of_primitive = function
  | Int _
  | Char _ ->
      `Int
  | Bool _ -> `Bool

let type_of_id ctx i =
  Result.of_option (Context.find ctx i) ~error:Type.Unbound

(** [type_check_primitive ctx p] is [Ok expr] where [expr] is a
    decorated expression node for [p]. *)
let type_check_primitive ctx p =
  Ok
    (Decorated.Ex.make (Decorated.Expr.Primitive p) ~ctx
       ~typ:(type_of_primitive p))

(** [type_check_id ctx i] is [Ok expr] where [expr] is a decorated
    expression node for [i] if [i] is in [ctx], or [Error err]
    otherwise. *)
let type_check_id ctx i =
  type_of_id ctx i >>| function
  | Var typ ->
      Ok
        (Decorated.Ex.make (Decorated.Expr.Id i) ~ctx
           ~typ:(typ :> Type.expr))
  | _ -> Error (Type.IdMismatch i)

(** [type_check_expr ctx enode] is [Ok expr] where [expr] is [enode]
    decorated within context [ctx] or [Error type_error] where
    [type_error] describes a semantic error of [enode] *)
let rec type_check_expr (ctx : Type.context) (enode : Expr.node) :
    Decorated.Expr.node Type.result =
  match Expr.Node.get enode with
  | Primitive p -> type_check_primitive ctx p
  | Id i -> type_check_id ctx i
  | Array arr -> type_check_array ctx arr
  | String s -> type_check_string ctx s
  | Bop (op, e1, e2) -> type_check_bop ctx op e1 e2
  | Uop (op, e) -> type_check_uop ctx op e
  | FnCall call -> type_check_fncall ctx call
  | Length node -> type_check_length ctx node
  | Index i -> type_check_index ctx i

and type_check_array ctx arr = failwith "unimplemented"

and type_check_string ctx str =
  Ok
    (Decorated.Ex.make (Decorated.Expr.String str) ~ctx
       ~typ:(`Array `Int))

and type_check_bop ctx op e1 e2 =
  let%bind dec1 = type_check_expr ctx e1 in
  let%bind dec2 = type_check_expr ctx e2 in
  let dec_bop = Decorated.Expr.Bop (op, dec1, dec2) in
  match (op, Decorated.Ex.typ dec1, Decorated.Ex.typ dec2) with
  | (Plus | Minus | Mult | HighMult | Div | Mod), `Int, `Int ->
      Ok (Decorated.Ex.make dec_bop ~ctx ~typ:`Int)
  | (Lt | Leq | Gt | Geq), `Int, `Int
  | (And | Or), `Bool, `Bool ->
      Ok (Decorated.Ex.make dec_bop ~ctx ~typ:`Bool)
  | (Eq | Neq), t1, t2 ->
      assert_eq t1 t2 >>| fun () ->
      Decorated.Ex.make dec_bop ~ctx ~typ:`Bool
  | _ -> failwith "unimplemented"

and type_check_uop ctx op e =
  let%bind dec = type_check_expr ctx e in
  let dec_uop = Decorated.Expr.Uop (op, dec) in
  match (op, Decorated.Ex.typ dec) with
  | IntNeg, `Int -> Ok (Decorated.Ex.make dec_uop ~ctx ~typ:`Int)
  | LogicalNeg, `Bool -> Ok (Decorated.Ex.make dec_uop ~ctx ~typ:`Bool)
  | _ -> Error OpMismatch
(* TODO add op + type to opmismatch *)

and type_check_fncall ctx call = failwith "unimplemented"
and type_check_length ctx node = failwith "unimplemented"
and type_of_array ctx arr = failwith "unimplemented"

and type_check_index ctx (e1, e2) =
  let%bind dec1 = type_check_expr ctx e1 in
  let%bind dec2 = type_check_expr ctx e2 in
  let dec_index = Decorated.Expr.Index (dec1, dec2) in
  match (Decorated.Ex.typ dec1, Decorated.Ex.typ dec2) with
  | `Array arr, `Int ->
      Ok (Decorated.Ex.make dec_index ~ctx ~typ:(type_of_array ctx arr))
  | _ -> Error (Mismatch (Decorated.Ex.typ dec1, Decorated.Ex.typ dec2))

(** [bool_or_error ctx e] is [Ok e] if [e] is [Ok e] and [e] has bool
    type in context [ctx] and [Error Mismatch] otherwise *)
and bool_or_error ctx e =
  let%bind e = type_check_expr ctx e in
  assert_bool (Decorated.Ex.typ e) >>| fun () -> e

(** [bool_or_error_stmt fn_ctx e] is [Ok e] if [e] is [Ok e] and [e] has
    bool type in function context [fn_ctx] and [Error Mismatch]
    otherwise *)
let bool_or_error_stmt fn_ctx =
  bool_or_error (Context.Fn.context fn_ctx)

(** [lub_stmt s1 s2] is [lub t1 t2] if [s1] has type [t1] and [s2] has
    type [t2] *)
let lub_stmt s1 s2 = lub (Decorated.St.typ s1) (Decorated.St.typ s2)

(** [unit_stmt s ~ctx] is [Decorated.St.make s ~ctx ~typ:`Unit] *)
let unit_stmt s ~ctx = Decorated.St.make s ~ctx ~typ:`Unit

(** [type_check_stmt fn_ctx snode] is [Ok stmt] where [stmt] is [snode]
    decorated within function context [fn_ctx] or [Error type_error]
    where [type_error] describes a semantic error of [snode] *)
let rec type_check_stmt (fn_ctx : Context.fn) (snode : Stmt.node) :
    Decorated.Stmt.node Type.result =
  match Stmt.Node.get snode with
  | If (e, s) -> type_check_if fn_ctx e s
  | IfElse (e, s1, s2) -> failwith "unimplemented"
  | While (e, s) -> failwith "unimplemented"
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

and type_check_cond ctx e s =
  let%bind e = bool_or_error_stmt ctx e in
  let%map s = type_check_stmt ctx s in
  (e, s)

and make_cond ~f ctx e s =
  let%map e, s = type_check_cond ctx e s in
  unit_stmt ~ctx (f e s)

and type_check_if fn_ctx e s =
  make_cond ~f:(fun e s -> Decorated.Stmt.If (e, s)) fn_ctx e s

and type_check_if_else fn_ctx e s1 s2 =
  let%bind e, s1 = type_check_cond fn_ctx e s1 in
  let%map s2 = type_check_stmt fn_ctx s2 in
  let typ = lub_stmt s1 s2 in
  let s = Decorated.Stmt.IfElse (e, s1, s2) in
  Decorated.St.make s ~ctx:fn_ctx ~typ

and type_check_while fn_ctx e s =
  make_cond ~f:(fun e s -> Decorated.Stmt.While (e, s)) fn_ctx e s
