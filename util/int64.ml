open Core
open Int64

type t = int64

(** [of_intable ~f x] is [Core.Int64.of_int (f x)] *)
let of_intable ~f x = of_int (f x)

let of_char = of_intable ~f:int_of_char
let of_uchar = of_intable ~f:Uchar.to_scalar
let of_bool = of_intable ~f:Bool.to_int
