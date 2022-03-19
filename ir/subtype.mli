type label = string

type 'expr dest =
  [ `Mem of 'expr
  | `Temp of label
  ]

type 'expr expr =
  [ `Const of int64
  | `Bop of Op.t * 'expr * 'expr
  | `Not of 'expr
  | `Name of label
  | 'expr dest
  ]

type 'expr stmt =
  [ `Move of 'expr dest * 'expr
  | `Jump of 'expr
  | `Label of label
  | `Return of 'expr list
  ]