open Core

module Lazy = struct
  let ok_if_true ~error b = if b then Ok () else Error (error ())

  let of_option ~error = function
    | Some v -> Ok v
    | None -> Error (error ())
end

let compose_ok f x = Ok (f x)
let join_error = function Error e -> e | Ok _ as ok -> ok
let ( >>? ) r f = Core.Result.map_error r ~f