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
  (** [print_cfg ~out ~f ~phase directive] prints cfgs for each function
      in [directive] based on [f] *)
  let print_cfg ~out ~f ~phase =
    let open Generic.Asm in
    function
    | Directive.Text fns ->
        let f fn =
          let out =
            Printf.sprintf "%s_%s_%s.dot"
              (Filename.chop_extension out)
              (Fn.name fn) phase
          in
          let open Graph.Directed.IntDigraph.Vertex in
          Fn.body fn |> Generic.create_cfg
          |> Graph.Directed.IntDigraph.graphviz
               ~string_of_vertex:(fun v ->
                 Printf.sprintf "%d: %s" (key v) (v |> value |> f))
               ~string_of_weight:(fun _ -> "")
          |> Util.File.println ~out
        in
        List.iter ~f fns
    | _ -> ()

  (** [print_cfgs ~out ~f ~phase asm] prints cfgs for each directive in
      [asm] based on [f] *)
  let print_cfgs ~out ~f ~phase =
    List.iter ~f:(print_cfg ~out ~f ~phase)

  (** [print_initial_cfgs ~out source] prints unoptimized concrete
      assembly cfgs for each function in [source] *)
  let print_initial_cfgs ~out source =
    let gensym = Ir.Gensym.create () in
    let unoptimized =
      source
      |> Ir.translate ~opt:Opt.disabled ~gensym
      |> Abstract.munch ~gensym:(Ir.Gensym.Temp.generator gensym)
      |> RegAlloc.allocate ~opt:Opt.disabled
    in
    print_cfgs ~out ~f:Concrete.to_string unoptimized ~phase:"initial"

  (** [print_source ~src ~out ~opt source] prints [source] to [out] as
      concrete assembly *)
  let print_source ~src ~out ~opt source =
    let open Opt in
    if opt.optir.initial then
      Ir.Output.print_initial ~out ~compunit:(Util.File.base src) source;
    if opt.optcfg.initial then print_initial_cfgs ~out source;
    let gensym = Ir.Gensym.create () in
    let ir = Ir.translate ~opt ~gensym source in
    if opt.optir.final then
      Ir.Output.print_final ~out ~compunit:(Util.File.base src) ~opt ir;
    let asm =
      ir
      |> Abstract.munch ~gensym:(Ir.Gensym.Temp.generator gensym)
      |> RegAlloc.allocate ~opt
    in
    if opt.optcfg.final then
      print_cfgs ~out ~f:Concrete.to_string asm ~phase:"final";
    asm |> Concrete.Asm.to_string |> Util.File.println ~out

  let file_to_file ?cache ~src ~out ~deps ~opt () =
    let ok = print_source ~src ~out ~opt in
    Ir.Output.iter_source ?cache ~src ~deps ~ok ()

  module Abstract = struct
    (** [print_source ~src ~out ~opt source] prints [source] to [out] as
        abstract assembly *)
    let print_source ~src ~out ~opt source =
      let gensym = Ir.Gensym.create () in
      let abstract =
        source
        |> Ir.translate ~opt ~gensym
        |> Abstract.munch ~gensym:(Ir.Gensym.Temp.generator gensym)
      in
      abstract |> Abstract.Asm.to_string |> Util.File.println ~out

    let file_to_file ?cache ~src ~out ~deps ~opt () =
      let ok = print_source ~src ~out ~opt in
      Ir.Output.iter_source ?cache ~src ~deps ~ok ()
  end
end
