open Core
open Option.Monad_infix

type arith = Arith.t [@@deriving hash, compare, sexp]

module Arith = Arith

type eq = Eq.t [@@deriving hash, compare, sexp]

module Eq = Eq

type log = Log.t [@@deriving hash, compare, sexp]

module Log = Log

type ord = Ord.t [@@deriving hash, compare, sexp]

module Ord = Ord

type cmp = Cmp.t [@@deriving hash, compare, sexp]

module Cmp = Cmp

type t =
  [ arith
  | cmp
  | log
  ]
[@@deriving hash, compare, sexp]

let to_string = function
  | `Mul -> "*"
  | `HMul -> "*>>"
  | `Div -> "/"
  | `Mod -> "%"
  | `Add -> "+"
  | `Sub -> "-"
  | `Lt -> "<"
  | `Leq -> "<="
  | `Geq -> ">="
  | `Gt -> ">"
  | `Eq -> "=="
  | `Neq -> "!="
  | `And -> "&"
  | `Or -> "|"

(** [eval_int op i1 i2] is [Some (i1 op i2)] if [op] is applicable to
    ints and the operation succeeds, or [None] otherwise *)
let eval_int op i1 i2 =
  match op with
  | #arith as op -> Arith.eval op i1 i2 >>| Primitive.Base.int
  | #cmp as op -> Some (`Bool (Cmp.eval op i1 i2))
  | #log -> None

(** [eval_bool op b1 b2] is [Some (b1 op b2)] if [op] is applicable to
    bools, or [None] otherwise *)
let eval_bool op b1 b2 =
  match op with
  | #eq as op -> Some (Eq.eval ~equal:Bool.equal op b1 b2)
  | #log as op -> Some (Log.eval op b1 b2)
  | #arith | #ord -> None

let eval_primitive op x1 x2 =
  match (Primitive.cast x1, Primitive.cast x2) with
  | `Int i1, `Int i2 -> eval_int op i1 i2
  | `Bool b1, `Bool b2 -> eval_bool op b1 b2 >>| Primitive.Base.bool
  | `Int _, `Bool _ | `Bool _, `Int _ -> None

let eval_array op a1 a2 =
  match op with `Add -> Some (a1 @ a2) | #t -> None
