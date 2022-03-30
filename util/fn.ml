open Core

let ( >> ) g f x = f (g x)
let ( << ) = Fn.compose
