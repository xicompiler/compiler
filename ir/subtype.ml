open Ast.Op

type label = string
type temp = [ `Temp of string ]
type 'expr call = [ `Call of int * 'expr * 'expr list ]

type 'expr dest =
  [ `Mem of 'expr
  | temp
  ]

type 'expr expr =
  [ `Const of int64
  | `Bop of Op.t * 'expr * 'expr
  | `Name of label
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
let log_neg e = `Bop (`Xor, e, one)
