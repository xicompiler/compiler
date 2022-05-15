open Core
open Frontend
open Util.Fn
module Operand = Operand
module Generic = Generic
module Abstract = Abstract
module Concrete = Concrete
module ConditionCode = ConditionCode
module RegAlloc = RegAlloc

module Output = struct
  (** [print_instrs ~out ~opt ~f source] prints [source] to [out] based
      on [f] *)
  let print_instrs ~out ~opt ~f source =
    let gensym = Ir.Gensym.create () in
    source
    |> Ir.translate ~opt ~gensym
    |> Abstract.munch ~gensym:(Ir.Gensym.Temp.generator gensym)
    |> f |> Util.File.println ~out

  (** [print_source ~out ~opt source] prints [source] to [out] as
      concrete assembly *)
  let print_source =
    print_instrs ~f:(RegAlloc.allocate >> Concrete.Asm.to_string)

  let file_to_file ?cache ~src ~out ~deps ~opt () =
    let ok = print_source ~out ~opt in
    Ir.Output.iter_source ?cache ~src ~deps ~ok ()

  module Abstract = struct
    (** [print_source ~out ~opt source] prints [source] to [out] as
        abstract assembly *)
    let print_source = print_instrs ~f:Abstract.Asm.to_string

    let file_to_file ?cache ~src ~out ~deps ~opt () =
      let ok = print_source ~out ~opt in
      Ir.Output.iter_source ?cache ~src ~deps ~ok ()
  end
end
