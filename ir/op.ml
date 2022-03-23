open Core
open Option.Let_syntax
open Int64
open Binop

type bitwise = [ `Xor ]

type shift =
  [ `LShift
  | `RShift
  | `ARShift
  ]

type unsigned =
  [ `ULt
  | `ULeq
  | `UGt
  | `UGeq
  ]

type t =
  [ bitwise
  | unsigned
  | Binop.t
  ]

let coerce bop = (bop :> t)

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
