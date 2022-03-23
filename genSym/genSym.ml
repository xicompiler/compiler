open Core

type 'a format = (int -> 'a, unit, string) Core.format

let generate_map ?(init = 0) fmt ~f =
  let counter = ref init in
  fun () ->
    let sym = Printf.sprintf fmt !counter in
    incr counter;
    f sym

let generate ?init fmt = generate_map ?init fmt ~f:Fn.id
