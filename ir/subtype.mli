open Ast.Op

(** [Label] represents a label in Xi *)
module Label : sig
  type t = string
  (** [t] is the type of a label in Xi *)

  val generator : unit -> unit -> t
  (** [generator ()] is a generator for creating fresh labels *)
end

type label = Label.t
(** [label] is an alias for [Label.t] *)

(** [Temp] represents a temporary variable in Xi *)
module Temp : sig
  type t = [ `Temp of string ]
  (** [t] represents the type of a temporary variable in Xi *)

  val generator : unit -> unit -> [> t ]
  (** [generator ()] is a generator for creating fresh temps *)
end

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
