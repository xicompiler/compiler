open Subtype

type expr =
  [ `Call of expr * expr list
  | `ESeq of stmt * expr
  | expr Subtype.expr
  ]

and stmt = expr Subtype.stmt

let one : expr = `Const Int64.one
let translate_enode e = failwith "unimplemented"

let translate_uop uop e =
  let e = translate_enode e in
  match uop with
  | `IntNeg -> `Bop (`Add, `Not e, one)
  | `LogNeg -> `Bop (`Xor, e, one)

let translate_bop (bop : Binop.t) e1 e2 : expr =
  `Bop (Op.coerce bop, translate_enode e1, translate_enode e2)
