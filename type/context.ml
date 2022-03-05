open Core
open Result.Let_syntax
open Conversions
open Definitions
open TypeError
module Map = Map.Make (String)

type context = id Map.t

type t = {
  context : id Map.t;
  ret : term;
}

let empty = { context = Map.empty; ret = `Unit }
let ret { ret } = ret
let with_ret ~ret ctx = { ctx with ret }

let find ~id { context } =
  Result.of_option (Map.find context id) ~error:(Unbound id)

let find_var ~id ctx =
  match%bind find ~id ctx with
  | Var typ -> Ok typ
  | Fn _ -> Error ExpectedTau

let find_fn ~id ctx =
  match%bind find ~id ctx with
  | Var _ -> Error ExpectedFn
  | Fn (t1, t2) -> Ok (t1, t2)

let add ~id ~typ ctx =
  match Map.add ~key:id ~data:typ ctx.context with
  | `Ok context -> Ok { ctx with context }
  | `Duplicate -> Error (Bound id)

let add_var ~id ~typ = add ~id ~typ:(Var typ)

let add_fn ~id ~arg ~ret =
  let typ = Fn (arg, ret) in
  add ~id ~typ

let add_fn_list ~id ~arg ~ret =
  let arg = term_of_tau_list arg in
  let ret = term_of_tau_list ret in
  add_fn ~id ~arg ~ret
