open Core

module Opt = struct
  type phases = {
    initial : bool;
    final : bool;
  }

  type t = {
    optir : phases;
    optcfg : phases;
    cf : bool;
    reg : bool;
    copy : bool;
    dce : bool;
    cp : bool;
    vn : bool;
  }

  (** [enabled opt] is the optimization-related arguments if
      optimizations are enabled *)
  let enabled opt =
    {
      opt with
      cf = true;
      reg = true;
      copy = true;
      dce = true;
      cp = true;
      vn = true;
    }

  (** [disabled] is the optimization-related arguments if optimizations
      are disabled *)
  let disabled =
    {
      optir = { initial = false; final = false };
      optcfg = { initial = false; final = false };
      cf = false;
      reg = false;
      copy = false;
      dce = false;
      cp = false;
      vn = false;
    }

  let phases_of_list =
    let init = { initial = false; final = false } in
    let f acc = function
      | "initial" -> { acc with initial = true }
      | "final" -> { acc with final = true }
      | phase ->
          Printf.printf "%s is not a supported phase." phase;
          acc
    in
    List.fold ~init ~f

  let config disable opt =
    if disable then disabled
    else
      let { cf; reg; copy; dce; cp; vn } = opt in
      if cf || reg || copy || dce || cp || vn then opt else enabled opt
end

type t = {
  files : string list;
  src_dir : string;
  lib_dir : string;
  std_dir : string;
  diag_out_dir : string;
  asm_out_dir : string;
  lex : bool;
  parse : bool;
  typecheck : bool;
  irgen : bool;
  irrun : bool;
  abstract_asm : bool;
  asmrun : bool;
  opt : Opt.t;
  target : string;
}
