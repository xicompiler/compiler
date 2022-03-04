open Core
open Result.Let_syntax
open Definitions
open TypeError
include Map.Make (String)

type context = id t

let find ~id ctx = Result.of_option (find ctx id) ~error:(Unbound id)

let find_var ~id ctx =
  match%bind find ~id ctx with
  | Var typ -> Ok typ
  | Fn _ -> Error ExpectedTau

let find_fun ~id ctx =
  match%bind find ~id ctx with
  | Var _ -> Error ExpectedFun
  | Fn (t1, t2) -> Ok (t1, t2)

module Fn = struct
  type t = {
    context : context;
    ret : term;
  }

  (** [map_find ~f ~id ctx] is [f ~id ctx.context] *)
  let map_find ~f ~id ctx = f ~id ctx.context

  let find = map_find ~f:find
  let find_var = map_find ~f:find_var
  let find_fun = map_find ~f:find_fun

  let add ~id ~typ ctx =
    match add ~key:id ~data:typ ctx.context with
    | `Ok context -> Ok { ctx with context }
    | `Duplicate -> Error (Bound id)

  let add_var ~id ~typ = add ~id ~typ:(Var typ)
  let context { context } = context
  let ret { ret } = ret
end

type fn = Fn.t