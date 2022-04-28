open Core
open Frontend
open Util.Fn
module Operand = Operand
module Generic = Generic
module Abstract = Abstract
module Concrete = Concrete
module ConditionCode = ConditionCode

module Output = struct
  (** [print_instrs ~out ~optimize ~f source] prints [source] to [out]
      based on [f] *)
  let print_instrs ~out ~optimize ~f source =
    let gensym = Ir.Gensym.create () in
    source
    |> Ir.translate ~optimize ~gensym
    |> Abstract.munch ~gensym:(Ir.Gensym.Temp.generator gensym)
    |> f |> Util.File.println ~out

  (** [print_source ~out ~optimize source] prints [source] to [out] as
      concrete assembly *)
  let print_source =
    print_instrs ~f:(Trivial.reg_alloc >> Concrete.Asm.to_string)

  let file_to_file ?cache ~src ~out ~deps ~optimize () =
    let ok = print_source ~out ~optimize in
    Ir.Output.iter_source ?cache ~src ~deps ~ok ()

  module Abstract = struct
    (** [print_source ~out ~optimize source] prints [source] to [out] as
        abstract assembly *)
    let print_source = print_instrs ~f:Abstract.Asm.to_string

    let file_to_file ?cache ~src ~out ~deps ~optimize () =
      let ok = print_source ~out ~optimize in
      Ir.Output.iter_source ?cache ~src ~deps ~ok ()
  end
end
