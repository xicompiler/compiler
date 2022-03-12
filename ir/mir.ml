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