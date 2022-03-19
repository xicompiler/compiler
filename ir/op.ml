type log =
  [ Binop.log
  | `Xor
  ]

type arith = Binop.Arith.base

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
  [ log
  | arith
  | unsigned
  | Binop.cmp
  ]
