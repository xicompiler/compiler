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

let empty = { context = Map.empty; ret = `Unit }
let ret { ret } = ret
let with_ret ~ret ctx = { ctx with ret }

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

let add_fn ~id ~arg ~ret =
  let typ = Fn (arg, ret) in
  add ~id ~typ

let add_fn_list ~id ~arg ~ret =
  let arg = term_of_tau_list arg in
  let ret = term_of_tau_list ret in
  add_fn ~id ~arg ~ret
