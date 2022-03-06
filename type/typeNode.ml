open Core
open Result.Let_syntax
open Definitions
open TypeError
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
  val make : 'a -> ctx:Context.t -> typ:typ -> pos:Position.t -> 'a t
  val assert_eq : expect:typ -> 'a t -> unit TypeError.Positioned.result

  val positioned :
    error:TypeError.error -> 'a t -> TypeError.Positioned.error
end

(** [Make (Args)] is a concrete Node with concrete types wrapped in
    [Args] *)
module Make (Args : Params) = struct
  include Args

  type 'a t = {
    value : 'a;
    context : Context.t;
    typ : typ;
    position : Position.t;
  }

  let context { context } = context
  let typ { typ } = typ
  let get { value } = value

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
    expect |> typ_equal got |> ok_if_true_lazy ~error
end

module Expr = struct
  include Make (struct
    type typ = expr

    let typ_equal t1 t2 =
      match (t1, t2) with
      | `Tuple ts1, `Tuple ts2 -> List.equal Tau.equal ts1 ts2
      | `Tuple _, _ | _, `Tuple _ -> false
      | _, _ -> Tau.equal (t1 : expr :> tau) (t2 : expr :> tau)

    let mismatch ~expect got = Mismatch (expect, got)
  end)

  let mismatch_sub ~expect got =
    mismatch ~expect:(expect :> typ) (got :> typ)

  let assert_eq_sub ~expect = assert_eq ~expect:(expect :> typ)
  let assert_int expr = assert_eq_sub ~expect:`Int expr
  let assert_bool expr = assert_eq_sub ~expect:`Bool expr

  let assert_eq_tau e1 e2 =
    let%bind t = Conversions.tau_of_expr_res (typ e1) (position e1) in
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
  let lub s1 s2 = lub (typ s1) (typ s2)
end

module Toplevel = struct
  type 'a t = {
    value : 'a;
    context : Context.t;
    position : Position.t;
  }

  let get { value } = value
  let context { context } = context
  let position { position } = position
  let make ~ctx ~pos value = { value; context = ctx; position = pos }
end

type 'a stmt = 'a Stmt.t
