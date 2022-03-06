open Core
open Result.Let_syntax
open Type
open Type.Error
open CtxError
open Util.Result
module Map = Map.Make (String)

type key = id

type t = {
  context : bound Map.t;
  ret : term;
}
[@@deriving sexp_of]

(** [init context] is a context with context [context] and [`Unit]
    return type *)
let init context = { context; ret = `Unit }

let empty = init Map.empty
let pr_int_array = Bound.fn_decl ~arg:Tau.int_array ()

(** [io_bindings] describe the context created by the [io] module*)
let io_bindings =
  [
    ("print", pr_int_array);
    ("println", pr_int_array);
    ("readln", Bound.fn_decl ~ret:Tau.int_array ());
    ("getchar", Bound.fn_decl ~ret:`Int ());
    ("eof", Bound.fn_decl ~ret:`Bool ());
  ]

(** [of_list bindings] is a context with return type [`Unit] and
    bindings from identifiers to [term]s described by [bindings] *)
let of_list bindings = bindings |> Map.of_alist_exn |> init

let io = of_list io_bindings
let int_bool = `Tuple [ `Int; `Bool ]

let conv_bindings =
  [
    ("parseInt", Bound.fn_decl ~arg:Tau.int_array ~ret:int_bool ());
    ("unparseInt", Bound.fn_decl ~arg:`Int ~ret:Tau.int_array ());
  ]

let conv = of_list conv_bindings
let ret { ret } = ret
let with_ret ~ret ctx = { ctx with ret = (ret :> term) }

let find ~id { context } =
  let key = Node.Position.get id in
  key |> Map.find context |> Lazy.of_option ~error:(fun () -> bound id)

let find_var ~id ctx =
  match%bind find ~id ctx with
  | `Var typ -> Ok typ
  | `FnDecl _ | `FnDefn _ -> Error (expected_tau id)

let find_fn ~id ctx =
  match%bind find ~id ctx with
  | `Var _ -> Error (expected_fn id)
  | `FnDecl { arg; ret } | `FnDefn { arg; ret } -> Ok (arg, ret)

let add_var ~id ~typ ctx =
  let key = Node.Position.get id in
  match Map.add ~key ~data:(`Var typ) ctx.context with
  | `Ok context -> Ok { ctx with context }
  | `Duplicate -> Error (bound id)

(** [set ~id ~data ctx] associates [id] with [data] in [ctx], replacing
    any exisiting binding *)
let set ~id ~data ctx =
  let key = Node.Position.get id in
  { ctx with context = Map.set ~key ~data ctx.context }

(** [set_fn_defn ~id ~fn ctx] associates [id] with function definition
    [fn] in [ctx], replacing any exisiting binding *)
let set_fn_defn ~id ~fn = set ~id ~data:(`FnDefn fn)

(** [find_opt ~id ctx] is [Some v] if [id] is associated with [v] in
    [ctx], or [None] otherwise *)
let find_opt ~id { context } = Map.find context (Node.Position.get id)

(** [try_add_fn ~mkctx ~id ~fn bound] is [Error] if [bound] is a
    variable or function definition, [Ok (mkctx ())] if the function
    declaration of [bound] matches the signature of [fn], and [Error] if
    they mismatch. *)
let try_add_fn ~mkctx ~id ~fn = function
  | `Var _ | `FnDefn _ -> Error (bound id)
  | `FnDecl decl ->
      if FnType.matches fn decl then Ok (mkctx ())
      else Error (fn_mismatch id)

(** [add_fn ~mkbound ~mkctx ~id ~fn ctx] is [ctx] associated with
    [mkbound fn] with [id] if unbound, [Error] if [id] is already bound
    to a function definition or variable, [Error] if [id] is associated
    with a function declaration incompatible with [fn], ir
    [Ok (mkctx ())] if they are compatible. *)
let add_fn ~mkbound ~mkctx ~id ~fn ctx =
  match find_opt ~id ctx with
  | Some bound -> try_add_fn ~mkctx ~id ~fn bound
  | None -> Ok (set ~id ~data:(mkbound fn) ctx)

(** [add_fn_decl ~id ~fn ctx] attempts to associate [fn] with [id] in
    context [ctx], failing on error conditions specified in [add_fn] *)
let add_fn_decl ~id ~fn ctx =
  let mkbound fn = `FnDecl fn in
  let mkctx () = ctx in
  add_fn ~mkbound ~mkctx ~id ~fn ctx

(** [add_fn_defn ~id ~fn ctx] attempts to associate [fn] with [id] in
    context [ctx], failing on error conditions specified in [add_fn] *)
let add_fn_defn ~id ~fn ctx =
  let mkbound fn = `FnDefn fn in
  let mkctx () = set_fn_defn ~id ~fn ctx in
  add_fn ~mkbound ~mkctx ~id ~fn ctx

(* [of_args f ~id ~arg ~ret] is [f ~id ~fn] where [fn] is a function
   with return type [ret] and argument types [arg] *)
let of_args f ~id ~arg ~ret =
  let fn = FnType.make ~arg ~ret () in
  f ~id ~fn

let add_fn_decl ~id ~arg ~ret = of_args add_fn_decl ~id ~arg ~ret
let add_fn_defn ~id ~arg ~ret = of_args add_fn_defn ~id ~arg ~ret

(** [of_list f ~id ~arg:args ~ret:rets] is [f ~id ~arg ~ret] where [arg]
    is the [term] representation of [args] and [ret] is the [term]
    representation of [rets] *)
let of_list f ~id ~arg ~ret =
  let arg = term_of_tau_list arg in
  let ret = term_of_tau_list ret in
  f ~id ~arg ~ret

let add_fn_decl_list = of_list add_fn_decl
let add_fn_defn_list = of_list add_fn_defn
