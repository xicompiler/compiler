open Core
open Result.Let_syntax
open Conversions
open Definitions
open TypeError
module Map = Map.Make (String)

type key = string Node.Position.t

type t = {
  context : id Map.t;
  ret : term;
}
[@@deriving sexp_of]

(** [init context] is a context with context [context] and [`Unit]
    return type *)
let init context = { context; ret = `Unit }

let empty = init Map.empty
let pr_int_array = Id.proc Tau.int_array

(** [io_bindings] describe the context created by the [io] module*)
let io_bindings =
  [
    ("print", pr_int_array);
    ("println", pr_int_array);
    ("readln", Id.fn_unit Tau.int_array);
    ("getchar", Id.fn_unit `Int);
    ("eof", Id.fn_unit `Bool);
  ]

(** [of_list bindings] is a context with return type [`Unit] and
    bindings from identifiers to [term]s described by [bindings] *)
let of_list bindings = bindings |> Map.of_alist_exn |> init

let io = of_list io_bindings
let int_bool = `Tuple [ `Int; `Bool ]

let conv_bindings =
  [
    ("parseInt", Id.fn ~arg:Tau.int_array ~ret:int_bool);
    ("unparseInt", Id.fn ~arg:`Int ~ret:Tau.int_array);
  ]

let conv = of_list conv_bindings
let ret { ret } = ret
let with_ret ~ret ctx = { ctx with ret = (ret :> term) }

let find ~id { context } =
  let key = Node.Position.get id in
  let error = Node.Position.error ~cause:(Unbound key) id in
  key |> Map.find context |> Result.of_option ~error

let find_var ~id ctx =
  match%bind find ~id ctx with
  | Var typ -> Ok typ
  | Fn _ -> Error (Node.Position.error ~cause:ExpectedTau id)

let find_fn ~id ctx =
  match%bind find ~id ctx with
  | Var _ -> Error (Node.Position.error ~cause:ExpectedFn id)
  | Fn (t1, t2) -> Ok (t1, t2)

let add ~id ~typ ctx =
  let key = Node.Position.get id in
  match Map.add ~key ~data:typ ctx.context with
  | `Ok context -> Ok { ctx with context }
  | `Duplicate -> Error (Node.Position.error ~cause:(Bound key) id)

let add_var ~id ~typ = add ~id ~typ:(Var typ)
let add_fn_defn ~id ~arg ~ret = add ~id ~typ:(Id.fn ~arg ~ret)

let add_fn_defn_list ~id ~arg ~ret =
  let arg = term_of_tau_list arg in
  let ret = term_of_tau_list ret in
  add_fn_defn ~id ~arg ~ret
