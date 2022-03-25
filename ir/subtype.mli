open Ast.Op

type label = string
(** [label] is the type of a label in Xi *)

type temp = [ `Temp of string ]
(** [temp] represents the type of a temporary variable in Xi *)

type 'expr call = [ `Call of int * 'expr * 'expr list ]
(** ['expr call] represents a function or procedure call in Xi *)

type 'expr dest =
  [ temp
  | `Mem of 'expr
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
  | `Label of label
  | `Jump of 'expr
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

module Infix : sig
  type 'expr binop = ([> 'expr expr ] as 'expr) -> 'expr -> 'expr

  val ( + ) : 'expr binop
  val ( * ) : 'expr binop
  val ( - ) : 'expr binop
  val ( := ) : 'a -> 'b -> [> `Move of 'a * 'b ]
  val ( ! ) : 'a -> [> `Mem of 'a ]
end
