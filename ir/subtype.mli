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

type 'expr call = [ `Call of 'expr * 'expr list ]
(** ['expr call] represents a function or procedure call in Xi *)

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

type 'expr cjump2 = [ `CJump of 'expr * label * label ]
(** ['expr cjump2] represents a conditional jump on expression of type
    ['expr] to a true label or false label*)

type 'expr stmt =
  [ 'expr call
  | `Move of 'expr dest * 'expr
  | `Jump of 'expr
  | `Label of label
  | `Return of 'expr list
  ]
(** [stmt] is an alias for [Stmt.t] *)

val log_neg : ([> 'expr expr ] as 'expr) -> 'expr
(** [log_neg e] is the IR node representing the logical negation of [e] *)
