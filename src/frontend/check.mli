open Core

(** [Error] represents a semantic error *)
module Error : sig
  type t =
    [ Parse.error
    | `SemanticError of Type.Error.Positioned.t
    ]

  val string_of_cause : Type.Error.t -> string
  (** [string_of_cause e] is the error message corresponding to [e] *)

  val to_string : string -> t -> string
  (** [to_string filename e] is the cli error message for the semantic
      error [e] in [filename] *)
end

type cache = (Ast.Undecorated.intf option, exn) result String.Table.t
(** [cache] caches interfaces used in type-checking *)

type error = Error.t
(** [error] is an alias for [Error.t] *)

type nonrec result = (Ast.Decorated.t, error) result
(** [result] is the type of a result from type-checking; either a
    decorated AST, or error *)

type dependencies = {
  lib_dir : string;
  std_dir : string;
}
(** [dependecies] represents a type for representing the external
    dependencies for a file *)

val type_check :
  file:string ->
  ?cache:cache ->
  deps:dependencies ->
  Ast.Undecorated.t ->
  result
(** [type_check ~cache { lib_dir; std_dir } lb] returns [Ok ast] where
    [ast] is the decorated ast of [lb], or [Error err] where [err] is a
    parsing error or type error. If [cache] is [Some tbl], AST nodes are
    memoized in [tbl]. If not provided, [lib_dir] defaults to ".".
    [std_dir] is used to resolve references to the standard library. *)

val type_check_file :
  ?cache:cache -> deps:dependencies -> string -> result File.Xi.result
(** same as [type_check] but takes a filename as argument *)

module Diagnostic : sig
  module Error : Util.Stringable.S with type t := error
  (** [Error] represents a Diagnostic Error *)

  val iter_file :
    ?cache:cache ->
    src:string ->
    deps:dependencies ->
    ?ok:(Ast.Decorated.t -> unit) ->
    ?err:(error -> unit) ->
    unit ->
    result File.Xi.result
  (** [iter_file ?cache ~src ~out ~deps ?ok ?err ()] is
      [iter_result ~ok ~err] if the file is a valid Xi file. *)

  val file_to_file :
    ?cache:cache ->
    src:string ->
    out:string ->
    deps:dependencies ->
    unit ->
    result File.Xi.result
  (** [file_to_file ~cache ~src ~out { lib_dir; src_dir }] writes
      checking diagnostic information to a file located at path [out],
      reading from file at path [src]. It yields [Ok ()] on success and
      [Error e] on failure. If not provided, [lib_dir] defaults to ".".
      If [cache] is [Some tbl], AST nodes are memoized in [tbl].
      [std_dir] is used to resolve references to the standard library. *)
end
