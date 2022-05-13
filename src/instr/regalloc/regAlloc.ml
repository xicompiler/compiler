open Core
open Generic
open Asm.Directive

let allocate_fn fn =
  let offset = Reg.Abstract.Table.create () in
  let allocate_fn = Trivial.allocate_fn in
  let body = allocate_fn ~offset fn in
  let temps = Int64.of_int (Hashtbl.length offset * 8) in
  Enter (temps, 0L) :: body

let allocate_directive = function
  | (Data _ | Globl _ | IntelSyntax _) as dir -> dir
  | Text fns -> Text (List.map ~f:(Asm.Fn.map_body ~f:allocate_fn) fns)

let allocate = List.map ~f:allocate_directive