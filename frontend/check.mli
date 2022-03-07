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

val type_check :
  lib_dir:string -> Lexing.lexbuf -> (Ast.Decorated.t, error) result
(** [type_check lib_dir lb] returns [Ok ast] where [ast] is the
    decorated ast of [lb], or [Error err] where [err] is a parsing error
    or type error. *)

module Diagnostic : sig
  val to_file : lib_dir:string -> Lexing.lexbuf -> string -> unit
  (** [to_file lexbuf out] checks [lexbuf] and writes the results to
      file at path [out] *)

  val file_to_file :
    src:string -> lib_dir:string -> out:string -> unit File.Xi.result
  (** [file_to_file ~src ~out] writes checking diagnostic information to
      a file located at path [out], reading from file at path [src]. It
      yields [Ok ()] on success and [Error e] on failure, where [e] is a
      string describing the failure. *)
end