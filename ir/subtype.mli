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

(** [Stmt] represents the subtype of an IR statement *)
module Stmt : sig
  type 'expr base =
    [ `Move of 'expr dest * 'expr
    | `Jump of 'expr
    | `Label of label
    | `Return of 'expr list
    ]
  (** [base] represents a base IR statement, excluding CJump's *)

  type 'expr t =
    [ 'expr base
    | `CJump of 'expr * label * label
    ]
  (** [t] represents the subtype of an IR statement, including a CJump
      with true label and false label*)
end

type 'expr stmt = 'expr Stmt.t
(** [stmt] is an alias for [Stmt.t] *)
