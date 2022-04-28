open Core

type t =
  [ Ord.t
  | Eq.t
  ]

let eval = function
  | #Ord.t as ord -> Ord.eval ord
  | #Eq.t as eq -> Eq.eval ~equal:Int64.equal eq
