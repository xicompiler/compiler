open Core
module Table = Ir.Temp.Virtual.Table

type t = Mem.Abstract.t Table.t

let create () = Table.create ()
let count = Hashtbl.length

let find m =
  let count = count m in
  let default () =
    let offset = Int64.of_int (-8 * succ count) in
    Mem.create ~offset `rbp
  in
  Hashtbl.find_or_add m ~default
