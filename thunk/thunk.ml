open Core

type 'a t = unit -> 'a

let map2 f = Tuple2.map ~f ((), ())
let map3 f = Tuple3.map ~f ((), (), ())
