open Core
open Option.Let_syntax
open Int64
open Binop

type base =
  [ `Add
  | `Sub
  | `Div
  | `Mul
  ]
[@@deriving hash, compare, sexp]

type arith =
  [ base
  | `HMul
  | `Mod
  ]
[@@deriving hash, compare, sexp]

type comp =
  [ `Lt
  | `Leq
  | `Geq
  | `Gt
  | `Eq
  | `Neq
  ]
[@@deriving hash, compare, sexp]

type log =
  [ `And
  | `Or
  ]
[@@deriving hash, compare, sexp]

type bitwise = [ `Xor ] [@@deriving hash, compare, sexp]

type shift =
  [ `LShift
  | `RShift
  | `ARShift
  ]
[@@deriving hash, compare, sexp]

type unsigned =
  [ `ULt
  | `ULeq
  | `UGt
  | `UGeq
  ]
[@@deriving hash, compare, sexp]

type t =
  [ bitwise
  | unsigned
  | arith
  | comp
  | log
  ]
[@@deriving hash, compare, sexp]

type negatable =
  [ `Lt
  | `Leq
  | `Geq
  | `Gt
  | `Eq
  | `Neq
  ]
[@@deriving hash, compare, sexp]

let coerce bop = (bop :> t)

let to_string = function
  | `Add -> "ADD"
  | `Sub -> "SUB"
  | `Mul -> "MUL"
  | `HMul -> "HMUL"
  | `Div -> "DIV"
  | `Mod -> "MOD"
  | `And -> "AND"
  | `Or -> "OR"
  | `Xor -> "XOR"
  | `RShift -> "RSHIFT"
  | `LShift -> "LSHIFT"
  | `ARShift -> "ARSHIFT"
  | `Lt -> "LT"
  | `Leq -> "LEQ"
  | `Geq -> "GEQ"
  | `Gt -> "GT"
  | `Eq -> "EQ"
  | `Neq -> "NEQ"
  | `ULt -> "ULT"
  | `ULeq -> "ULEQ"
  | `UGt -> "UGT"
  | `UGeq -> "UGEQ"

let log_neg : negatable -> negatable = function
  | `Lt -> `Geq
  | `Leq -> `Gt
  | `Geq -> `Lt
  | `Gt -> `Leq
  | `Eq -> `Neq
  | `Neq -> `Eq

(** [int_of_bool b] is [one] if b or [zero] otherwise *)
let int_of_bool b = if b then one else zero

(** [op_of_shift op] is the inline int64 shift operator for [op] *)
let op_of_shift = function
  | `LShift -> ( lsl )
  | `RShift -> ( lsr )
  | `ARShift -> ( asr )

(** [signed_of_unsigned op] is the signed version of the unsigned
    operator [op] *)
let signed_of_unsigned = function
  | `ULt -> ( < )
  | `ULeq -> ( <= )
  | `UGt -> ( > )
  | `UGeq -> ( >= )

(** [compare_neg_nonneg op] is the result of [i1 op i2], where [i1] is
    negative and [i2] is nonnegative *)
let compare_neg_nonneg = function
  | `ULt -> false
  | `ULeq -> false
  | `UGt -> true
  | `UGeq -> true

(** [eval_unsigned op i1 i2] is the result of an unsigned operation
    [i1 op i2] *)
let eval_unsigned (op : unsigned) i1 i2 =
  match (is_negative i1, is_negative i2) with
  | true, true | false, false -> (signed_of_unsigned op) i1 i2
  | true, false -> compare_neg_nonneg op
  | false, true -> not (compare_neg_nonneg op)

let eval (op : t) i1 i2 =
  match op with
  | `Xor -> Some (Int64.bit_xor i1 i2)
  | #unsigned as op -> Some (eval_unsigned op i1 i2 |> int_of_bool)
  | #arith as op -> Arith.eval op i1 i2
  | #cmp as op -> Some (Cmp.eval op i1 i2 |> int_of_bool)
  | #log as op -> Some (Log.eval_bits op i1 i2)

type cmp =
  [ unsigned
  | comp
  ]
[@@deriving hash, compare, sexp]
