open Core
open Result.Monad_infix

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

let map_nsf ~f =
  Fn.compose (Result.map_error ~f:Error.nosuchfile) (Main.map ~f)

type 'a map = Lexing.lexbuf -> 'a

let map ~source ~intf file =
  match Caml.Filename.extension file with
  | ".xi" | ".rh" -> map_nsf ~f:source file >>| Either.first
  | ".ixi" | ".ri" -> map_nsf ~f:intf file >>| Either.second
  | _ -> Error (`NotXiFile file)

let map_same ~source ~intf file =
  map ~source ~intf file >>| Either.value

let map_same_fn ~f = map_same ~source:f ~intf:f
