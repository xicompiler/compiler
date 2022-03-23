include module type of Main
module Xi : module type of Xi

(** [Diagnostic] represents the module type of a Diagnostic function *)
module type Diagnostic = sig
  val file_to_file : src:string -> out:string -> unit Xi.result
  (** [file_to_file ~src ~out] writes parsing diagnostic information to
      a file located at path [out], reading from file at path [src]. It
      yields [Ok ()] on success and [Error e] on failure. *)
end