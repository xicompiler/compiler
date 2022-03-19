open Subtype

type expr =
  [ `Call of expr * expr list
  | `ESeq of stmt * expr
  | expr Subtype.expr
  ]

and stmt =
  [ expr Subtype.stmt
  | `CJump of expr * label * label
  ]

let one : expr = `Const Int64.one
let sixty_four : expr = `Const (Int64.of_int 64)
let translate_enode e = failwith "unimplemented"

let translate_uop uop e =
  let e = translate_enode e in
  match uop with
  | `IntNeg -> `Bop (`Add, `Not e, one)
  | `LogNeg -> `Bop (`Xor, e, one)

let translate_bop bop e1 e2 =
  let e1 = translate_enode e1 in
  let e2 = translate_enode e2 in
  match bop with
  | #Binop.base as bop -> `Bop ((bop :> Op.t), e1, e2)
  | `HighMult -> `ARShift (`Bop (`Mult, e1, e2), sixty_four)
