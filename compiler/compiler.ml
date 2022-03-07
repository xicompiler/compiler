open Core
open Frontend
open Util.File
module Args = Args

type nonrec result = (unit, string list) result

(** [replace_ext ~ext ~file] is the string [chopped.ext] where [chopped]
    is [file] with its extension removed. *)
let replace_ext ~ext ~file = Filename.chop_extension file ^ ext

(** [make_out_path ~dir ~file ~ext] is the path of parse diagnostics
    file ending in [ext] in directory [dir] for file [file] *)
let make_out_path ~dir ~file ext =
  let file = replace_ext ~file ~ext in
  Filename.concat dir file

(** [create_out_path ~dir ~file ext] creates the directory of
    [make_out_path ~dir ~file ext] if absent and returns the result. *)
let create_out_path ~dir ~file ext =
  let path = make_out_path ~dir ~file ext in
  Core.Unix.mkdir_p (Filename.dirname path);
  path

open Args

(** [diagnostic ext ~f ~args ~file lexbuf] applies diagnostic function
    [f] to lexbuf [lexbuf], writing the results to output file [file]
    with extension [ext] in output directory [args.out_dir] *)
let diagnostic ext ~f ~args ~file lexbuf =
  ext |> create_out_path ~dir:args.out_dir ~file |> f lexbuf

(** [lex ~dir ~file lexbuf] applies lexing diagnostic function lexbuf
    [lexbuf], writing the results to output file [file] in output
    directory [dir] *)
let lex = diagnostic ".lexed" ~f:Lex.Diagnostic.to_file

(** [parse start ~dir ~file lexbuf] applies the parsing diagnostic
    function tp lexbuf [lexbuf] from start symbol [start], writing the
    results to output file [file] in output directory [dir] *)
let parse start =
  diagnostic ".parsed" ~f:(Parse.Diagnostic.to_file ~start)

(** [deps_of_args args] are the semantic dependecies corresponding to
    [args] *)
let deps_of_args { lib_dir; std_dir; _ } : Check.dependencies =
  { lib_dir; std_dir }

(** [typecheck deps lexbuf] applies the parsing diagnostic function tp
    lexbuf [lexbuf] from start symbol [start], writing the results to
    output file [file] in output directory [dir] *)
let typecheck ?cache ~args =
  let deps = deps_of_args args in
  diagnostic ".typed" ~f:(Check.Diagnostic.to_file ?cache ~deps) ~args

(** [src_path_of_args ~args file] is the concatenation of [args.src_dir]
    and [file] *)
let src_path_of_args ~args = Filename.concat args.src_dir

(** [map_file ~args f] composes [f] with [src_path_of_args args] *)
let map_file ~args f = Fn.compose f (src_path_of_args ~args)

(** [lex_file ~args file] applies the lexing diagnostic function to file
    at [args.src_path], writing the results to output file [file] in
    output directory [args.out_dir] *)
let lex_file ~args file =
  map_file ~args (File.Xi.map_same ~f:(lex ~args ~file)) file

(** [parse_file ~args file] applies the parsing diagnostic function to
    file at [args.src_path], writing the results to output file [file]
    in output directory [args.out_dir] *)
let parse_file ~args file =
  map_file ~args (Parse.map ~f:(parse ~args ~file)) file

(** [typecheck_file ?cache ~args file] applies the typecheck diagnostic
    function to file at [args.src_path], writing the results to output
    file [file] in output directory [args.out_dir] *)
let typecheck_file ?cache ~args file =
  map_file ~args
    (File.Xi.map_same ~f:(typecheck ?cache ~args ~file))
    file

(** [compile_file ~args file] compiles file at path [file] and is
    [Ok ()] on success or [Error e] on failure, where [e] is an error
    message. *)
let compile_file ?cache ~args file =
  let src_path = src_path_of_args ~args file in
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
  if args.parse then ignore (parse_file ~args file);
  if args.lex then ignore (lex_file ~args file);
  if args.typecheck then ignore (typecheck_file ?cache ~args file);
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
