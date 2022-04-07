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

type name = [ `Name of label ]
(** [name] represents an expression that is the address of a label *)

type 'expr expr =
  [ name
  | `Const of int64
  | `Bop of Op.t * 'expr * 'expr
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

(** [Infix] contains useful infix operators that operate on expressions *)
module Infix : sig
  type 'expr binop = ([> 'expr expr ] as 'expr) -> 'expr -> 'expr
  (** ['expr binop] is the type of a binary operator operator on ['expr] *)

  val ( + ) : 'expr binop
  (** [e1 + e2] is [`Bop (`Add, e1, e2)] *)

  val ( * ) : 'expr binop
  (** [e1 * e2] is [`Bop (`Mult, e1, e2)] *)

  val ( - ) : 'expr binop
  (** [e1 - e2] is [`Bop (`Sub, e1, e2)] *)

  val ( < ) : 'expr binop
  (** [e1 < e2] is [`Bop (`Lt, e1, e2)] *)

  val ( <? ) : 'expr binop
  (** [e1 <? e2] is [`Bop (`ULt, e1, e2)] *)

  val ( := ) : 'a -> 'b -> [> `Move of 'a * 'b ]
  (** [e1 := e2] is [`Move (e1, e2)] *)

  val ( ! ) : 'a -> [> `Mem of 'a ]
  (** [!e] is [`Mem e] *)
end
