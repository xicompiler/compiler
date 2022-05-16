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

(** [t] is the type of an operator in IR *)

type negatable =
  [ `Lt
  | `Leq
  | `Geq
  | `Gt
  | `Eq
  | `Neq
  ]
[@@deriving hash, compare, sexp]
(** [negatable] is a negatable operator in IR *)

type cmp =
  [ unsigned
  | comp
  ]
[@@deriving hash, compare, sexp]

val coerce : [< t ] -> t
(** [coerce bop] coerces [bop] to a [t] *)

val log_neg : negatable -> negatable
(** [log_neg op] is the negated operator of [op] *)

val eval : t -> int64 -> int64 -> int64 option

include Util.Stringable.S with type t := t
