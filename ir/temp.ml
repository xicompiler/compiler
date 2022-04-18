open Core

type temp = [ `Temp of string ] [@@deriving sexp, compare, hash]

module Virtual = struct
  module Args = struct
    type t =
      [ temp
      | `Rv of int
      | `Arg of int
      ]
    [@@deriving sexp, compare, hash]
  end

  include Args

  let rv = Printf.sprintf "_RV%d"
  let arg = Printf.sprintf "_ARG%d"

  let to_string : [< t ] -> string = function
    | `Arg i -> arg i
    | `Rv i -> rv i
    | `Temp t -> t

  include Comparable.Make (Args)
  include Hashable.Make (Args)
end

type t = temp
