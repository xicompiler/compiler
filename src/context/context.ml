module Error = Error
open Error
open Core
open Result.Let_syntax
open Type
open Type.Error
open Util.Result

type key = id

type t = {
  context : bound String.Map.t;
  ret : term;
  beta : bool; (* beta is true if inside a while loop, false otherwise *)
}
[@@deriving sexp_of, fields]

let empty = { context = String.Map.empty; ret = `Unit; beta = false }
let with_ret ~ret ctx = { ctx with ret = (ret :> term) }
let with_beta ~beta ctx = { ctx with beta }

let find ~id { context } =
  id |> Entry.key |> Map.find context
  |> Lazy.of_option ~error:(fun () -> unbound id)

let find_var ~id ctx =
  match%bind find ~id ctx with
  | `Var typ -> Ok typ
  | `FnDecl _ | `FnDefn _ | `RecordDecl _ | `RecordDefn _ ->
      Error (expected_tau id)

let find_var_exn ~id ctx = ok (find_var ~id ctx)

let find_fn ~id ctx =
  match%bind find ~id ctx with
  | `Var _ | `RecordDecl _ | `RecordDefn _ -> Error (expected_fn id)
  | `FnDecl { arg; ret } | `FnDefn { arg; ret } -> Ok (arg, ret)

let find_fn_exn ~id ctx = ok (find_fn ~id ctx)

let find_record ~id ctx =
  match%bind find ~id ctx with
  | `Var _ | `FnDecl _ | `FnDefn _ -> Error (expected_record id)
  | `RecordDecl fields | `RecordDefn fields -> Ok fields

let find_record_exn ~id ctx = ok (find_record ~id ctx)

let add_var ~id ~typ ctx =
  let key = Entry.key id in
  match Map.add ~key ~data:(`Var typ) ctx.context with
  | `Ok context -> Ok { ctx with context }
  | `Duplicate -> Error (bound id)

(** [set ~id ~data ctx] associates [id] with [data] in [ctx], replacing
    any exisiting binding *)
let set ~id ~data ctx =
  let key = Entry.key id in
  { ctx with context = Map.set ~key ~data ctx.context }

(** [set_fn_defn ~id ~fn ctx] associates [id] with function definition
    [fn] in [ctx], replacing any exisiting binding *)
let set_fn_defn ~id ~fn = set ~id ~data:(`FnDefn fn)

(** [set_record_defn ~id ~record ctx] associates [id] with record
    definition [record] in [ctx], replacing any exisiting binding *)
let set_record_defn ~id ~record = set ~id ~data:(`RecordDefn record)

(** [find_opt ~id ctx] is [Some v] if [id] is associated with [v] in
    [ctx], or [None] otherwise *)
let find_opt ~id { context } = Map.find context (Entry.key id)

(** [try_add_fn ~mkctx ~id ~fn bound] is [Error] if [bound] is a
    variable or function definition, [Ok (mkctx ())] if the function
    declaration of [bound] matches the signature of [fn], and [Error] if
    they mismatch. *)
let try_add_fn ~mkctx ~id ~fn = function
  | `Var _ | `RecordDefn _ | `RecordDecl _ | `FnDefn _ ->
      Error (bound id)
  | `FnDecl decl ->
      if FnType.matches fn decl then Ok (mkctx ())
      else Error (fn_mismatch id)

let try_add_record ~mkctx ~id ~record = function
  | `Var _ | `FnDefn _ | `FnDecl _ | `RecordDefn _ -> Error (bound id)
  | `RecordDecl decl ->
      if RecordType.matches record decl then Ok (mkctx ())
      else Error (record_mismatch id)

(** [add_fn ~mkbound ~mkctx ~id ~fn ctx] is [ctx] associated with
    [mkbound fn] with [id] if unbound, [Error] if [id] is already bound
    to a function definition or variable, [Error] if [id] is associated
    with a function declaration incompatible with [fn], ir
    [Ok (mkctx ())] if they are compatible. *)
let add_fn ~mkbound ~mkctx ~id ~fn ctx =
  match find_opt ~id ctx with
  | Some bound -> try_add_fn ~mkctx ~id ~fn bound
  | None -> Ok (set ~id ~data:(mkbound fn) ctx)

let add_record ~mkbound ~mkctx ~id ~record ctx =
  match find_opt ~id ctx with
  | Some bound -> try_add_record ~mkctx ~id ~record bound
  | None -> Ok (set ~id ~data:(mkbound record) ctx)

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

let add_record_decl ~id ~record ctx =
  let mkbound record = `RecordDecl record in
  let mkctx () = ctx in
  add_record ~mkbound ~mkctx ~id ~record ctx

let add_record_defn ~id ~record ctx =
  let mkbound record = `RecordDefn record in
  let mkctx () = set_record_defn ~id ~record ctx in
  add_record ~mkbound ~mkctx ~id ~record ctx

(* [of_args f ~id ~arg ~ret] is [f ~id ~fn] where [fn] is a function
   with return type [ret] and argument types [arg] *)
let of_args f ~id ~arg ~ret =
  let fn = FnType.make ~arg ~ret () in
  f ~id ~fn

(* [of_fields f ~id ~fields] is [f ~id ~record] where [record] is a
   record with [fields] *)
let of_fields f ~id ~fields =
  let record = RecordType.make ~fields () in
  f ~id ~record

let add_fn_decl ~id ~arg ~ret = of_args add_fn_decl ~id ~arg ~ret
let add_fn_defn ~id ~arg ~ret = of_args add_fn_defn ~id ~arg ~ret
let add_record_decl ~id ~fields = of_fields add_record_decl ~id ~fields
let add_record_defn ~id ~fields = of_fields add_record_defn ~id ~fields
