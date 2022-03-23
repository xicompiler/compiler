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
(** An ['expr dest] is an expression that can be the target of a move *)

type 'expr expr =
  [ `Const of int64
  | `Bop of Op.t * 'expr * 'expr
  | `Name of label
  | 'expr dest
  ]
(** An ['expr expr] is the subtype of an IR expression *)

type 'expr stmt =
  [ 'expr call
  | `Move of 'expr dest * 'expr
  | `Jump of 'expr
  | `Label of label
  | `Return of 'expr list
  ]
(** ['expr base] is the base type of an IR statement *)

type 'expr cjump2 =
  [ 'expr stmt
  | `CJump of 'expr * label * label
  ]
(** ['expr cjump2] represents a statement in IR, including conditional
    jump on expression of type ['expr] to a true label or false label *)

val zero : [> 'expr expr ]
(** [zero] is the IR constant 0 *)

val one : [> 'expr expr ]
(** [one] is the IR constant 1 *)

val eight : [> 'expr expr ]
(** [eight] is the IR constant 8 *)

val log_neg : ([> 'expr expr ] as 'expr) -> 'expr
(** [log_neg e] is the IR node representing the logical negation of [e] *)
