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
  concat ~dir file

(** [create_out_path ~dir ~file ext] creates the directory of
    [make_out_path ~dir ~file ext] if absent and returns the result. *)
let create_out_path ~dir ~file ext =
  let path = make_out_path ~dir ~file ext in
  Core.Unix.mkdir_p (Filename.dirname path);
  path

(** [diagnostic ext ~f ~dir ~file lexbuf] applies diagnostic function
    [f] to lexbuf [lexbuf], writing the results to output file [file]
    with extension [ext] in output directory [dir] *)
let diagnostic ext ~f ~dir ~file lexbuf =
  ext |> create_out_path ~dir ~file |> f lexbuf

(** [lex_diagnostic ~dir ~file lexbuf] applies lexing diagnostic
    function lexbuf [lexbuf], writing the results to output file [file]
    in output directory [dir] *)
let lex_diagnostic = diagnostic ".lexed" ~f:Lex.Diagnostic.to_file

(** [parse_diagnostic start ~dir ~file lexbuf] applies the parsing
    diagnostic function tp lexbuf [lexbuf] from start symbol [start],
    writing the results to output file [file] in output directory [dir] *)
let parse_diagnostic start =
  diagnostic ".parsed" ~f:(Parse.Diagnostic.to_file ~start)

(** [typecheck_diagnostic start ~dir ~file lexbuf] applies the parsing
    diagnostic function tp lexbuf [lexbuf] from start symbol [start],
    writing the results to output file [file] in output directory [dir] *)
let typecheck_diagnostic ~lib_dir =
  diagnostic ".typed" ~f:(Check.Diagnostic.to_file ~lib_dir)

(** [lex_diagnostic_file ~dir ~file src_path] applies the lexing
    diagnostic function to file at [src_path], writing the results to
    output file [file] in output directory [dir] *)
let lex_diagnostic_file ~dir ~file =
  File.Xi.map_same ~f:(lex_diagnostic ~dir ~file)

(** [parse_diagnostic_file ~dir ~file src_path] applies the parsing
    diagnostic function to file at [src_path], writing the results to
    output file [file] in output directory [dir] *)
let parse_diagnostic_file ~dir ~file =
  Parse.map ~f:(parse_diagnostic ~dir ~file)

(** [typecheck_diagnostic_file ~dir ~file src_path] applies the
    typecheck diagnostic function to file at [src_path], writing the
    results to output file [file] in output directory [dir] *)
let typecheck_diagnostic_file ~lib_dir ~dir ~file =
  File.Xi.map_same ~f:(typecheck_diagnostic ~lib_dir ~dir ~file)

(** [compile_file file] compiles file at path [file] and is [Ok ()] on
    success or [Error e] on failure, where [e] is an error message. *)
let compile_file src_path lib_dir =
  let f lexbuf =
    lexbuf
    |> Check.type_check ~lib_dir
    |> Result.map_error ~f:(Check.Error.to_string src_path)
    |> Result.ignore_m
  in
  File.Xi.map_same ~f src_path

open Args

(** [compile_file_options args file] compiles file [file] with command
    line arguments [args] *)
let compile_file_options
    { lex; parse; typecheck; src_dir; out_dir; lib_dir; _ }
    file =
  let src_path = concat ~dir:src_dir file in
  if parse then
    ignore (parse_diagnostic_file ~dir:out_dir ~file src_path);
  if lex then ignore (lex_diagnostic_file ~dir:out_dir ~file src_path);
  if typecheck then
    ignore
      (typecheck_diagnostic_file ~lib_dir ~dir:out_dir ~file src_path);
  compile_file src_path lib_dir

(** [stringify res] is an error message string if [res] is an error *)
let stringify = function
  | Ok (Ok ()) -> Ok ()
  | Ok (Error s) -> Error s
  | Error e -> Error (File.Xi.Error.to_string e)

let compile args =
  args.files
  |> List.map ~f:(compile_file_options args)
  |> List.map ~f:stringify |> Result.combine_errors_unit
