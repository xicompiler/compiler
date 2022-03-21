open Core
open Result.Let_syntax
open Type
open Error
open Util.Result

module type Params = sig
  type typ

  val typ_equal : typ -> typ -> bool
  val mismatch : expect:typ -> typ -> error
end

module type S = sig
  include Params
  include ContextNode.S

  val typ : 'a t -> typ
  val make : 'a -> ctx:Ctx.t -> typ:typ -> pos:Position.t -> 'a t
  val assert_eq : expect:typ -> 'a t -> unit Positioned.result
  val positioned : error:error -> 'a t -> Positioned.error
end

(** [Make (Args)] is a concrete Node with concrete types wrapped in
    [Args] *)
module Make (Args : Params) = struct
  include Args

  module B = struct
    type 'a t = {
      value : 'a;
      context : Ctx.t;
      typ : typ;
      position : Position.t;
    }

    let get { value } = value
    let set ~value node = { node with value }
  end

  include Node.Make (B)
  open B

  let context { context } = context
  let typ { typ } = typ

  let make value ~ctx ~typ ~pos =
    { value; context = ctx; typ; position = pos }

  let position { position } = position

  let positioned ~error { position } =
    Positioned.make ~pos:position error

  let assert_eq ~expect v =
    let got = typ v in
    let error () =
      let pos = position v in
      let cause = mismatch ~expect got in
      Positioned.make ~pos cause
    in
    expect |> typ_equal got |> Lazy.ok_if_true ~error
end

module Expr = struct
  include Make (struct
    type typ = expr

    let typ_equal = Expr.equal
    let mismatch ~expect got = Mismatch (expect, got)
  end)

  let mismatch_sub ~expect got =
    mismatch ~expect:(expect :> typ) (got :> typ)

  let assert_eq_sub ~expect = assert_eq ~expect:(expect :> typ)
  let assert_int expr = assert_eq_sub ~expect:`Int expr
  let assert_bool expr = assert_eq_sub ~expect:`Bool expr

  let assert_eq_tau e1 e2 =
    let%bind t = tau_of_expr_res (typ e1) (position e1) in
    assert_eq_sub ~expect:t e2
end

type 'a expr = 'a Expr.t

module Stmt = struct
  include Make (struct
    type typ = stmt

    let typ_equal = Poly.equal
    let mismatch ~expect got = StmtMismatch (expect, got)
  end)

  let make_unit = make ~typ:`Unit
  let make_void = make ~typ:`Void
  let assert_unit stmt = assert_eq ~expect:`Unit stmt
  let assert_void stmt = assert_eq ~expect:`Void stmt
  let lub s1 s2 = Stmt.lub (typ s1) (typ s2)
end

module Toplevel = struct
  module B = struct
    type 'a t = {
      value : 'a;
      context : Ctx.t;
      position : Position.t;
    }

    let get { value } = value
    let set ~value node = { node with value }
  end

  include Node.Make (B)
  open B

  let context { context } = context
  let position { position } = position
  let make ~ctx ~pos value = { value; context = ctx; position = pos }

  let of_pos_node ~ctx ~node value =
    { value; context = ctx; position = Node.Position.position node }
end

type 'a stmt = 'a Stmt.t
