open Core

(** [Error] represents a semantic error *)
module Error : sig
  type t = Type.Error.Positioned.error

  val to_string : string -> t -> string
  (** [to_string filename e] is the cli error message for the semantic
      error [e] in [filename] *)
end

module Diagnostic : sig
  val file_to_file : src:string -> out:string -> unit XiFile.result
  (** [file_to_file ~src ~out] writes checking diagnostic information to
      a file located at path [out], reading from file at path [src]. It
      yields [Ok ()] on success and [Error e] on failure, where [e] is a
      string describing the failure. *)
end