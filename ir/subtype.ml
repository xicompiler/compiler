open Ast.Op

type binop = Bop

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

module Stmt = struct
  type 'expr base =
    [ `Move of 'expr dest * 'expr
    | `Jump of 'expr
    | `Label of label
    | `Return of 'expr list
    ]

  type 'expr t =
    [ 'expr base
    | `CJump of 'expr * label * label
    ]
end

type 'expr stmt = 'expr Stmt.t
