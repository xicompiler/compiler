open Core
open Frontend
open Util.Fn
module Operand = Operand
module Generic = Generic
module Abstract = Abstract
module Concrete = Concrete
module ConditionCode = ConditionCode

let ir_to_file ~gensym ~out source =
  let f oc =
    Abstract.munch ~gensym source
    |> Trivial.reg_alloc |> Concrete.Asm.to_string
    |> Printf.fprintf oc "%s\n"
  in
  Out_channel.with_file ~f out

let deps : Check.dependencies =
  {
    std_dir = Util.File.stdlib;
    lib_dir = "./test/typecheck/interfaces";
  }

let get_source = function
  | Ast.Source src -> src
  | Ast.Intf _ -> failwith ""

let dummy_gensym =
  let counter = ref 1000 in
  fun () ->
    incr counter;
    Printf.sprintf "t%d" !counter

let string_to_file ~out s =
  s |> Lexing.from_string |> Parse.parse_prog |> Caml.Result.get_ok
  |> Check.type_check ~deps |> Caml.Result.get_ok |> get_source
  |> Ir.translate ~optimize:true
  |> ir_to_file ~gensym:dummy_gensym ~out

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
    let f = Ast.iter_source ~f:(print_source ~out ~optimize) in
    Check.Diagnostic.iter_file ?cache ~src ~out ~deps ~f ()

  module Abstract = struct
    (** [print_source ~out ~optimize source] prints [source] to [out] as
        abstract assembly *)
    let print_source = print_instrs ~f:Abstract.Asm.to_string

    let file_to_file ?cache ~src ~out ~deps ~optimize () =
      let f = Ast.iter_source ~f:(print_source ~out ~optimize) in
      Check.Diagnostic.iter_file ?cache ~src ~out ~deps ~f ()
  end
end
