open Core

module Error = struct
  type t = Type.Error.Positioned.error

  (** [desc e] is the error description corresponding to [e] *)
  let desc = Type.Error.to_string

  let string_of_cause cause = cause |> desc |> Printf.sprintf "error:%s"

  (** [fmt] is the format for a lexical error message *)
  let fmt = format_of_string "Semantic error beginning at %s:%s"

  let to_string filename error =
    let pos = Position.Error.position error in
    error |> Position.Error.cause |> desc
    |> Position.Error.format pos
    |> Printf.sprintf fmt filename
end

module Diagnostic = struct
  let file_to_file ~src ~out = failwith "unimplemented"
end