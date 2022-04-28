type t = int64

let of_int i = `Imm (Int64.of_int i)