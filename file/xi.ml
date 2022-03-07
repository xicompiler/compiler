open Core

module Error = struct
  type t =
    [ `NoSuchFile of string
    | `NotXiFile of string
    ]
  [@@deriving variants]

  let not_xi_file = Printf.sprintf "%s: Not a Xi file"

  let to_string = function
    | `NoSuchFile file -> Main.no_such_file file
    | `NotXiFile file -> not_xi_file file
end

type error = Error.t
type nonrec 'a result = ('a, error) result

let map_nsf ~f file =
  file |> Main.map ~f |> Result.map_error ~f:Error.nosuchfile

let map ~source ~intf file =
  match Caml.Filename.extension file with
  | ".xi" -> map_nsf ~f:source file
  | ".ixi" -> map_nsf ~f:intf file
  | _ -> Error (`NotXiFile file)

type 'a map = Lexing.lexbuf -> 'a

let map_same ~f = map ~source:f ~intf:f
