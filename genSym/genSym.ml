open Core

type 'a format = (int -> 'a, unit, string) Core.format

let create ?(init = 0) fmt =
  let counter = ref init in
  fun () ->
    let sym = Printf.sprintf fmt !counter in
    incr counter;
    sym
