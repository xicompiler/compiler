open Core
open Ast.Op

type label = string [@@deriving sexp]
type 'expr call = [ `Call of int * 'expr * 'expr list ]

type 'expr dest =
  [ Temp.t
  | `Mem of 'expr
  ]

type name = [ `Name of label ] [@@deriving sexp]

type 'expr expr =
  [ name
  | `Const of int64
  | `Bop of Op.t * 'expr * 'expr
  | Temp.Virtual.t
  | 'expr dest
  ]

type 'expr stmt =
  [ 'expr call
  | `Move of 'expr dest * 'expr
  | `Label of label
  | `Jump of 'expr
  | `Return of 'expr list
  ]

type 'expr cjump2 =
  [ 'expr stmt
  | `CJump of 'expr * label * label
  ]

let zero = `Const Int64.zero
let one = `Const Int64.one
let eight = `Const 8L

let log_neg = function
  | `Bop ((#Op.negatable as op), e1, e2) ->
      `Bop ((Op.log_neg op :> Op.t), e1, e2)
  | e -> `Bop (`Xor, e, one)

module Infix = struct
  type 'expr binop = ([> 'expr expr ] as 'expr) -> 'expr -> 'expr

  let ( + ) e1 e2 = `Bop (`Add, e1, e2)
  let ( < ) e1 e2 = `Bop (`Lt, e1, e2)
  let ( <? ) e1 e2 = `Bop (`ULt, e1, e2)
  let ( * ) e1 e2 = `Bop (`Mul, e1, e2)
  let ( - ) e1 e2 = `Bop (`Sub, e1, e2)
  let ( := ) e1 e2 = `Move (e1, e2)
  let ( ! ) e = `Mem e
end
