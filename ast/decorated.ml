open Core
open Result.Let_syntax
open Util.Result
open Type.Error

module Data = struct
  module type S = sig
    type t

    val context : t -> Context.t
    val position : t -> Position.t
  end

  module type Term = sig
    include S

    type typ

    val typ : t -> typ
  end

  (** [Typ] is the module type of terms needed to construct decorated
      data *)
  module type Typ = sig
    include Equal.S

    val mismatch : expect:t -> t -> Type.Error.t
  end

  (** [Make (Args)] is a concrete Node with concrete types wrapped in
      [Args] *)
  module Make (Typ : Typ) = struct
    type typ = Typ.t

    type t = {
      context : Context.t;
      typ : typ;
      position : Position.t;
    }
    [@@deriving fields]

    let mismatch ~expect { typ = got; position = pos } =
      let cause = Typ.mismatch ~expect got in
      Position.Error.create ~pos cause

    let create ~typ ~ctx ~pos = { context = ctx; typ; position = pos }

    let assert_eq ~expect v =
      let error () = mismatch ~expect v in
      expect |> Typ.equal v.typ |> Lazy.ok_if_true ~error
  end

  module Expr = struct
    include Make (struct
      include Type.Expr

      let mismatch ~expect got = ExprMismatch (expect, got)
    end)

    let assert_eq ~expect = assert_eq ~expect:(expect :> typ)
    let create ~typ = create ~typ:(typ :> typ)
    let create_int = create ~typ:`Int
    let create_bool = create ~typ:`Bool
    let assert_int = assert_eq ~expect:`Int
    let assert_bool = assert_eq ~expect:`Bool

    let assert_array { typ; position } =
      let error () = Positioned.expected_array position in
      typ |> Type.Expr.is_array |> Lazy.ok_if_true ~error

    let to_tau { typ; position = pos } =
      let error () = Position.Error.create ~pos ExpectedTau in
      typ |> Type.Expr.to_tau |> Lazy.of_option ~error

    let join e ~expect =
      let%bind typ = to_tau e in
      let error () = mismatch ~expect:(expect :> typ) e in
      typ |> Type.Tau.join expect |> Lazy.of_option error
  end

  type expr = Expr.t

  module Stmt = struct
    include Make (struct
      include Type.Stmt

      let mismatch ~expect got = StmtMismatch (expect, got)
    end)

    let create_unit = create ~typ:`Unit
    let create_void = create ~typ:`Void
    let assert_unit = assert_eq ~expect:`Unit
    let assert_void = assert_eq ~expect:`Void
    let lub s1 s2 = Type.Stmt.lub (typ s1) (typ s2)
  end

  type stmt = Stmt.t

  module Toplevel = struct
    type t = {
      context : Context.t;
      position : Position.t;
    }
    [@@deriving fields]

    let create ~ctx ~pos = { context = ctx; position = pos }
  end

  type toplevel = Toplevel.t
end

include Types.Make (Data)

type result = t Type.Error.Positioned.result
