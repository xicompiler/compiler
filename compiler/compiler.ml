open Core
open Frontend
module Args = Args

type nonrec result = (unit, string list) result

(** [make_path ~dir file] is the string [dir/file] *)
let make_path ~dir file =
  match dir with
  | Some dir -> Printf.sprintf "%s/%s" dir file
  | None -> file

(** [replace_ext ~ext ~file] is the string [chopped.ext] where [chopped]
    is [file] with its extension removed. *)
let replace_ext ~ext ~file = Filename.chop_extension file ^ ext

(** [make_out_path ~dir ~file ~ext] is the path of parse diagnostics
    file ending in [ext] in directory [dir] for file [file] *)
let make_out_path ~dir ~file ext =
  let file = replace_ext ~file ~ext in
  make_path ~dir file

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

(** [parse_diagnostic_file ~dir ~file src_path] applies the parsing
    diagnostic function to file at [src_path], writing the results to
    output file [file] in output directory [dir] *)
let parse_diagnostic_file ~dir ~file =
  Parse.map ~f:(parse_diagnostic ~dir ~file)

(** [lex_diagnostic_file ~dir ~file src_path] applies the lexing
    diagnostic function to file at [src_path], writing the results to
    output file [file] in output directory [dir] *)
let lex_diagnostic_file ~dir ~file =
  XiFile.map_same ~f:(lex_diagnostic ~dir ~file)

(** [compile_file file] compiles file at path [file] and is [Ok ()] on
    success or [Error e] on failure, where [e] is an error message. *)
let compile_file src_path =
  let f start lexbuf =
    lexbuf |> Parse.parse ~start
    |> Result.map_error ~f:(Parse.Error.to_string src_path)
    |> Result.ignore_m
  in
  Parse.bind ~f src_path

open Args

(** [compile_file_options args file] compiles file [file] with command
    line arguments [args] *)
let compile_file_options { lex; parse; src_dir; out_dir; _ } file =
  let src_path = make_path ~dir:src_dir file in
  if parse then
    ignore (parse_diagnostic_file ~dir:out_dir ~file src_path);
  if lex then ignore (lex_diagnostic_file ~dir:out_dir ~file src_path);
  compile_file src_path

let compile args =
  args.files
  |> List.map ~f:(compile_file_options args)
  |> Result.combine_errors_unit
