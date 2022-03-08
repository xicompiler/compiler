open! Core

type fn = {
  arg : Term.t;
  ret : Term.t;
}
[@@deriving sexp_of]
(** [fn] represents an abstract function mapping type [arg] to type
    [ret] *)

type t =
  [ `FnDecl of fn
  | `FnDefn of fn
  ]
[@@deriving sexp_of]
(** [t] is either a function declaration or definition. *)

val make : ?arg:[< Term.t ] -> ?ret:[< Term.t ] -> unit -> fn
(** [make ~arg ~ret] is a [fn] taking argument type [arg], return type
    [ret]. If either of [arg] or [ret] are not provided, then they
    default to [`Unit]. *)

val decl : ?arg:[< Term.t ] -> ?ret:[< Term.t ] -> unit -> t
(** [decl ~arg ~ret ()] is a [Fn] declaration taking argument type [arg]
    and return type [ret] If either of [arg] or [ret] are not provided,
    then they default to [`Unit]. *)

val defn : ?arg:[< Term.t ] -> ?ret:[< Term.t ] -> unit -> t
(** [defn ~arg ~ret ()] is a [Fn] definition taking argument type [arg]
    and return type [ret] If either of [arg] or [ret] are not provided,
    then they default to [`Unit]. *)

val matches : fn -> fn -> bool
(** [matches t1 t2] is [true] iff , [Term.equal t1.arg t2.arg], and
    [Term.equal t1.ret t2.ret] *)
