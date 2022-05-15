open Core
open Generic
open Asm.Directive

let allocate_fn fn =
  let offset = Reg.Abstract.Table.create () in
  let allocate_instrs = Trivial.allocate_instrs in
  let body = allocate_instrs ~offset fn in
  let temps = Int64.of_int (Hashtbl.length offset * 8) in
  Enter (temps, 0L) :: body

let gensym =
  let counter = ref 3000 in
  fun () ->
    incr counter;
    Printf.sprintf "_t%d" !counter

let allocate_fn fn = Color.assign fn ~gensym

let allocate_directive = function
  | (Data _ | Globl _ | IntelSyntax _) as dir -> dir
  | Text fns -> Text (List.map ~f:(Asm.Fn.map_body ~f:allocate_fn) fns)

let allocate = List.map ~f:allocate_directive

module Color = Color