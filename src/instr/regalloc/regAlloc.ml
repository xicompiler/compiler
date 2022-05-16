open Core
open Generic
open Asm.Directive

let allocate_fn_unoptimized fn =
  let offset = Reg.Abstract.Table.create () in
  let allocate_instrs = Trivial.allocate_instrs in
  let body = allocate_instrs ~offset fn in
  let temps = Int64.of_int (Hashtbl.length offset * 8) in
  Enter (temps, 0L) :: body

let allocate_fn ~(opt : Opt.t) ~gensym =
  if opt.reg then Color.assign ~gensym else allocate_fn_unoptimized

(** [allocate_directive ~opt directive] allocates registers for the
    abstract variables in [directive] *)
let allocate_directive ~opt ~gensym = function
  | (Data _ | Globl _ | IntelSyntax _) as dir -> dir
  | Text fns ->
      Text
        (List.map
           ~f:(Asm.Fn.map_body ~f:(allocate_fn ~opt ~gensym))
           fns)

let allocate ~opt ~gensym =
  List.map ~f:(allocate_directive ~opt ~gensym)

module Color = Color
