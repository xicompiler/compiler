open Core
open Compiler
open Args

let usage_msg = "Usage: xic [options] <source files>"

(** [print_no_files ()] prints the warning message for no files. *)
let print_no_files () =
  print_endline "Warning: no files were specified."

(** [try_compile args] attempts to compile a program described by
    arguments [args], exiting with code 1 on error. *)
let try_compile
    ({ files; lex; parse; typecheck; irgen; irrun; _ } as args) =
  if List.is_empty files then print_no_files ();
  let iter_errors es =
    List.iter ~f:print_endline es;
    if not (lex || parse || typecheck || irgen || irrun) then exit 1
  in
  args |> compile |> Result.iter_error ~f:iter_errors

let command =
  Command.basic ~summary:usage_msg
    Command.Let_syntax.(
      let%map_open files =
        anon (sequence ("filename" %: Filename_unix.arg_type))
      and src_dir =
        flag "-sourcepath"
          (optional_with_default "." string)
          ~doc:" Specify where to find input source files."
      and lib_dir =
        flag "-libpath"
          (optional_with_default "." string)
          ~doc:" Specify where to find library interface files."
      and diag_out_dir =
        flag "-D"
          (optional_with_default "." string)
          ~doc:" Specify where to place generated diagnostic files."
      and asm_out_dir =
        flag "-d"
          (optional_with_default "." string)
          ~doc:
            " Specify where to place generated assembly output files."
      and lex =
        flag "--lex" no_arg
          ~doc:" Generate output from lexical analysis."
      and parse =
        flag "--parse" no_arg
          ~doc:" Generate output from syntactic analysis."
      and typecheck =
        flag "--typecheck" no_arg
          ~doc:" Generate output from semantic analysis."
      and irgen =
        flag "--irgen" no_arg ~doc:" Generate intermediate code."
      and irrun =
        flag "--irrun" no_arg
          ~doc:" Generate and interpret immediate code."
      and abstract_asm =
        flag "--abstract-asm" no_arg
          ~doc:" Generate abstract assembly code."
      and asmrun =
        flag "--asmrun" no_arg ~doc:" Run generated assembly code."
      and disable_optimize =
        flag "-O" no_arg ~doc:" Disable optimizations."
      and target =
        flag "-target"
          (optional_with_default "linux" string)
          ~doc:
            " Specify the operating system for which to generate code."
      in
      let args =
        {
          files;
          src_dir;
          lib_dir;
          std_dir = Util.File.stdlib;
          diag_out_dir;
          asm_out_dir;
          lex;
          parse;
          typecheck;
          irgen;
          irrun;
          abstract_asm;
          asmrun;
          disable_optimize;
          target;
        }
      in
      fun () -> try_compile args)

let () =
  Command_unix.run ~version:"1.0" ~build_info:"bfs45_dc854_vmj5_zak33"
    command
