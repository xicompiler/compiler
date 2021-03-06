open! Core

type t =
  [ FnType.t
  | RecordType.t
  | `Var of Tau.t
  ]
[@@deriving sexp_of]
(** [t] is a type used in an environment entry. *)

val var : Tau.t -> t
(** [var t] is [`Var t]*)

val fn_decl : ?arg:[< Term.t ] -> ?ret:[< Term.t ] -> unit -> t
(** [fn_decl ~arg ~ret ()] is a [Fn] declaration taking argument type
    [arg] and return type [ret] If either of [arg] or [ret] are not
    provided, then they default to [`Unit]. *)

val fn_defn : ?arg:[< Term.t ] -> ?ret:[< Term.t ] -> unit -> t
(** [fn_defn ~arg ~ret ()] is a [Fn] definition taking argument type
    [arg] and return type [ret] If either of [arg] or [ret] are not
    provided, then they default to [`Unit]. *)

val record_decl : ?fields:(string * Tau.t) list -> unit -> t
(** [record_decl fields ()] is a [Record] declaration with [fields]. If
    [fields] is not provided it defaults to the empty list. *)

val record_defn : ?fields:(string * Tau.t) list -> unit -> t
(** [record_defn fields ()] is a [Record] definition with [fields]. If
    [fields] is not provided it defaults to the empty list. *)
