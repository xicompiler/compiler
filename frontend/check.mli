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

type error = Error.t

type cache = Ast.Toplevel.intf option String.Table.t
(** [cache] caches interfaces used in type-checking *)

val type_check :
  ?cache:cache ->
  ?lib_dir:string ->
  Lexing.lexbuf ->
  (Ast.Decorated.t, error) result
(** [type_check ~cache ~lib_dir lb] returns [Ok ast] where [ast] is the
    decorated ast of [lb], or [Error err] where [err] is a parsing error
    or type error. If [cache] is [Some tbl], AST nodes are memoized in
    [tbl]. If not provided, [lib_dir] defaults to ".". *)

module Diagnostic : sig
  val to_file :
    ?cache:cache -> ?lib_dir:string -> Lexing.lexbuf -> string -> unit
  (** [to_file lexbuf out] checks [lexbuf] and writes the results to
      file at path [out]. If [cache] is [Some tbl], AST nodes are
      memoized in [tbl]. If not provided, [lib_dir] defaults to ".". *)

  val file_to_file :
    ?cache:cache ->
    ?lib_dir:string ->
    src:string ->
    out:string ->
    unit ->
    unit File.Xi.result
  (** [file_to_file ~cache ~lib_dir ~src ~out ()] writes checking
      diagnostic information to a file located at path [out], reading
      from file at path [src]. It yields [Ok ()] on success and
      [Error e] on failure, where [e] is a string describing the
      failure. If not provided, [lib_dir] defaults to ".". If [cache] is
      [Some tbl], AST nodes are memoized in [tbl]. *)
end