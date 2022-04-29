open Core
open Generic
open Asm.Directive

let reg_alloc_fn fn = failwith ""

let reg_alloc_directive = function
  | (Data _ | Globl _ | IntelSyntax _) as dir -> dir
  | Text fns -> Text (List.map ~f:(Asm.Fn.map_body ~f:reg_alloc_fn) fns)

let reg_alloc = List.map ~f:reg_alloc_directive
