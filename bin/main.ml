open Core
open Compiler
open Args

let usage_msg = "Usage: xic [options] <source files>"
let files = ref default.files
let out_dir = ref ""
let src_dir = ref ""
let lib_dir = ref ""
let lex = ref default.lex
let parse = ref default.parse
let typecheck = ref default.typecheck
let help = ref default.help

let speclist =
  [
    ( "-D",
      Arg.Set_string out_dir,
      "Specify where to place generated diagnostic files." );
    ( "-sourcepath",
      Arg.Set_string src_dir,
      "Specify where to find input source files." );
    ("--lex", Arg.Set lex, "Generate output from lexical analysis.");
    ("--parse", Arg.Set parse, "Generate output from syntactic analysis.");
    ("--typecheck", Arg.Set parse, "Generate output from semantic analysis.");
    ("--help", Arg.Set help, "Print a synopsis of options.");
    ("-help", Arg.Set help, "Print a synopsis of options.");
  ]

(** [print_help ()] prints the help message. *)
let print_help () = print_string (Arg.usage_string speclist usage_msg)

(** [try_compile args] attempts to compile a program described by
    arguments [args], exiting with code 1 on error. *)
let try_compile ({ files; lex; parse; help; _ } as args) =
  if List.is_empty files || help then print_help ();
  let iter_errors es =
    List.iter ~f:print_endline es;
    if not (lex || parse) then exit 1
  in
  args |> compile |> Result.iter_error ~f:iter_errors

let command =
  Command.basic ~summary:usage_msg
    Command.Let_syntax.(
      let%map_open files = anon (sequence ("filename" %: Filename.arg_type))
      and out_dir =
        flag "-D" (optional string)
          ~doc:"Specify where to place generated diagnostic files."
      and src_dir =
        flag "-sourcepath" (optional string)
          ~doc:"Specify where to find input source files."
      and lib_dir =
        flag "-libpath" (optional string)
          ~doc:" Specify where to find library interface files."
      and lex =
        flag "--lex" no_arg ~doc:" Generate output from lexical analysis."
      and parse =
        flag "--parse" no_arg ~doc:" Generate output from syntactic analysis."
      and typecheck =
        flag "--typecheck" no_arg
          ~doc:" Generate output from semantic analysis."
      in
      let args =
        {
          files;
          out_dir;
          src_dir;
          lib_dir;
          lex;
          parse;
          typecheck;
          help = false;
        }
      in
      fun () -> try_compile args)

let () = Command.run ~version:"1.0" ~build_info:"bfs45_dc854_vmj5_zak33" command
