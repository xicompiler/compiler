open! Core

type record = (string * Tau.t) list [@@deriving sexp_of]
(** [fn] represents an abstract function mapping type [arg] to type
    [ret] *)

type t =
  [ `RecordDecl of record
  | `RecordDefn of record
  ]
[@@deriving sexp_of]
(** [t] is either a function declaration or definition. *)

val make : ?fields:(string * Tau.t) list -> unit -> record
(** [make ~fields] is a [record] with [fields]. If [fields] is not
    provided, then it defaults to the empty list. *)

val decl : ?fields:(string * Tau.t) list -> unit -> t
(** [decl ~arg ~ret ()] is a [Fn] declaration taking argument type [arg]
    and return type [ret] If either of [arg] or [ret] are not provided,
    then they default to [`Unit]. *)

val defn : ?fields:(string * Tau.t) list -> unit -> t
(** [defn ~arg ~ret ()] is a [Fn] definition taking argument type [arg]
    and return type [ret] If either of [arg] or [ret] are not provided,
    then they default to [`Unit]. *)

val matches : record -> record -> bool
(** [matches t1 t2] is [true] iff , [Term.equal t1.arg t2.arg], and
    [Term.equal t1.ret t2.ret] *)
