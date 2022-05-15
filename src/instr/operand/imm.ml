open Core

type t = int64 [@@deriving sexp]

let of_int i = `Imm (Int64.of_int i)
