open Core

type 'a t = {
  asm : 'a Generic.t list;
  cost : int;
  temp : string;
  dfs : int;
}
[@@deriving fields]

let create ~asm ?(cost = 0) ~dfs temp = { asm; cost; temp; dfs }
let add_instr instr tiling = { tiling with asm = instr :: tiling.asm }