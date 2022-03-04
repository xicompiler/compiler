open Core
open Definitions
open TypeError

module type Params = sig
  type typ

  val typ_equal : typ -> typ -> bool
  val mismatch : expect:typ -> typ -> error

  type context
end

module type S = sig
  include Node.S
  include Params

  val context : 'a t -> context
  val typ : 'a t -> typ
  val make : 'a -> ctx:context -> typ:typ -> pos:Position.t -> 'a t
  val position : 'a t -> Position.t
  val positioned : error:error -> 'a t -> Positioned.error
  val assert_eq : expect:typ -> 'a t -> unit TypeError.Positioned.result
end

(** [Make (Args)] is a concrete Node with concrete types wrapped in
    [Args] *)
module Make (Args : Params) = struct
  include Args

  type 'a t = {
    value : 'a;
    context : context;
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

    let typ_equal = Poly.equal
    let mismatch ~expect got = Mismatch (expect, got)

    type context = Context.context
  end)

  let mismatch_sub ~expect got =
    mismatch ~expect:(expect :> typ) (got :> typ)

  let assert_eq_sub ~expect = assert_eq ~expect:(expect :> typ)
  let assert_int expr = assert_eq_sub ~expect:`Int expr
  let assert_bool expr = assert_eq_sub ~expect:`Bool expr
end

type 'a expr = 'a Expr.t

module Stmt = struct
  include Make (struct
    type typ = stmt

    let typ_equal = Poly.equal
    let mismatch ~expect got = StmtMismatch (expect, got)

    type context = Context.fn
  end)

  let make_unit = make ~typ:`Unit
  let make_void = make ~typ:`Void
  let assert_unit stmt = assert_eq ~expect:`Unit stmt
end

type 'a stmt = 'a Stmt.t
