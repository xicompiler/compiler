open Ast.Op

(** [label_fmt] is the format of labels, in accordance with the calling
    convention*)
let label_fmt = format_of_string "x%d"

module Label = struct
  type t = string

  let generator () = GenSym.generate label_fmt
end

type label = Label.t

let temp_fmt = format_of_string "l%d"

module Temp = struct
  type t = [ `Temp of string ] [@@deriving variants]

  let generator () = GenSym.generate_map temp_fmt ~f:temp
end

type 'expr call = [ `Call of 'expr * 'expr list ]

type 'expr dest =
  [ `Mem of 'expr
  | Temp.t
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
  | `Jump of 'expr
  | `Label of label
  | `Return of 'expr list
  ]

type 'expr cjump2 =
  [ 'expr stmt
  | `CJump of 'expr * label * label
  ]

let one = `Const Int64.one
let log_neg e = `Bop (`Xor, e, one)
