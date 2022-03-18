open Core

(** [Error] represents a semantic error *)
module Error : sig
  type t =
    [ Parse.error
    | `SemanticError of Type.Error.Positioned.error
    ]

  val string_of_cause : Type.error -> string
  (** [string_of_cause e] is the error message corresponding to [e] *)

  val to_string : string -> t -> string
  (** [to_string filename e] is the cli error message for the semantic
      error [e] in [filename] *)
end

type cache = (Ast.Toplevel.intf option, exn) result String.Table.t
(** [cache] caches interfaces used in type-checking *)

type error = Error.t
type nonrec result = (Ast.Decorated.t, error) result

type dependencies = {
  lib_dir : string;
  std_dir : string;
}
(** [dependecies] represents a type for representing the external
    dependencies for a fil e*)

val type_check : ?cache:cache -> deps:dependencies -> Ast.t -> result
(** [type_check ~cache { lib_dir; std_dir } lb] returns [Ok ast] where
    [ast] is the decorated ast of [lb], or [Error err] where [err] is a
    parsing error or type error. If [cache] is [Some tbl], AST nodes are
    memoized in [tbl]. If not provided, [lib_dir] defaults to ".".
    [std_dir] is used to resolve references to the standard library. *)

val type_check_file :
  ?cache:cache -> deps:dependencies -> string -> result File.Xi.result
(** same as [type_check] but takes a filename as argument *)

module Diagnostic : sig
  val file_to_file :
    ?cache:cache ->
    src:string ->
    out:string ->
    deps:dependencies ->
    unit ->
    unit File.Xi.result
  (** [file_to_file ~cache ~src ~out { lib_dir; src_dir }] writes
      checking diagnostic information to a file located at path [out],
      reading from file at path [src]. It yields [Ok ()] on success and
      [Error e] on failure. If not provided, [lib_dir] defaults to ".".
      If [cache] is [Some tbl], AST nodes are memoized in [tbl].
      [std_dir] is used to resolve references to the standard library. *)
end