open Core

module Lazy = struct
  let ok_if_true ~error b = if b then Ok () else Error (error ())

  let of_option ~error = function
    | Some v -> Ok v
    | None -> Error (error ())
end

let map_either ~ok ~error = function
  | Ok o -> Ok (ok o)
  | Error e -> Error (error e)

let compose_ok f x = Ok (f x)
let join_error = function Error e -> e | Ok _ as ok -> ok
let ( >>? ) r f = Core.Result.map_error r ~f