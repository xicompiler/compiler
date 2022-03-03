open Core
open Definitions
open TypeError
include Map.Make (String)

type context = id t

let find ~id ctx = Result.of_option (find ctx id) ~error:(Unbound id)

module Fn = struct
  type t = {
    context : context;
    ret : term;
  }

  let find ~id ctx = find ~id ctx.context

  let add ~id ~typ ctx =
    match add ~key:id ~data:typ ctx.context with
    | `Ok context -> Ok { ctx with context }
    | `Duplicate -> Error (Bound id)

  let add_var ~id ~typ = add ~id ~typ:(Var typ)
  let context { context } = context
  let ret { ret } = ret
end

type fn = Fn.t