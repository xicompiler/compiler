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
  | shift
  | unsigned
  | Binop.t
  ]

let coerce bop = (bop :> t)
