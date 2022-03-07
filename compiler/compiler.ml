open Core
open Frontend
open Util.File
module Args = Args

type nonrec result = (unit, string list) result

open Args

(** [lex_out ~dir src] writes the lexing diagnostic file to [src] *)
let lex_out ~dir ~src =
  Lex.Diagnostic.file_to_file ~src
    ~out:(Util.File.diagnostic ".lexed" dir src)

(** [parse_out ~dir src] writes the parsing diagnostic file to [src] *)
let parse_out ~dir ~src =
  Parse.Diagnostic.file_to_file ~src
    ~out:(Util.File.diagnostic ".lexed" dir src)

(** [check_out ~dir src] writes the typechecking diagnostic file to
    [src] *)
let check_out ?cache ~dir ~src ~deps () =
  Check.Diagnostic.file_to_file ?cache ~src
    ~out:(Util.File.diagnostic ".lexed" dir src)
    ~deps ()

(** [deps_of_args args] are the semantic dependecies corresponding to
    [args] *)
let deps_of_args { lib_dir; std_dir; _ } : Check.dependencies =
  { lib_dir; std_dir }

(** [compile_file ~args file] compiles file at path [file] and is
    [Ok ()] on success or [Error e] on failure, where [e] is an error
    message. *)
let compile_file ?cache ~args file =
  let src_path = Filename.concat args.src_dir file in
  let f lexbuf =
    lexbuf
    |> Check.type_check ?cache ~deps:(deps_of_args args)
    |> Result.map_error ~f:(Check.Error.to_string src_path)
    |> Result.ignore_m
  in
  File.Xi.map_same ~f src_path

(** [compile_file_options args file] compiles file [file] with command
    line arguments [args] *)
let compile_file_options ?cache ~args file =
  let dir = args.out_dir in
  let deps = deps_of_args args in
  if args.parse then ignore (parse_out ~dir ~src:file);
  if args.lex then ignore (lex_out ~dir ~src:file);
  if args.typecheck then
    ignore (check_out ?cache ~dir ~src:file ~deps ());
  compile_file ?cache ~args file

(** [stringify res] is an error message string if [res] is an error *)
let stringify = function
  | Ok ok -> ok
  | Error e -> Error (File.Xi.Error.to_string e)

let compile args =
  let cache = String.Table.create () in
  let invoke = compile_file_options ~cache ~args in
  args.files
  |> List.map ~f:(Fn.compose stringify invoke)
  |> Result.combine_errors_unit
