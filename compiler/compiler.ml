open Core
open Frontend

module Args = struct
  type t = {
    files : string list;
    out_dir : string option;
    src_dir : string option;
    lex : bool;
    parse : bool;
    help : bool;
  }

  let default =
    {
      files = [];
      out_dir = None;
      src_dir = None;
      lex = false;
      parse = false;
      help = false;
    }
end

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
  let file = replace_ext ~file ~ext:".lexed" in
  make_path ~dir file

(** [create_out_path ~dir ~file ext] creates the directory of
    [make_out_path ~dir ~file ext] if absent and returns the result. *)
let create_out_path ~dir ~file ext =
  let path = make_out_path ~dir ~file ext in
  Core.Unix.mkdir_p path;
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

(** [parse_diagnostic start ~dir ~file lexbuf] applies parsing
    diagnostic function lexbuf [lexbuf] from start symbol [start],
    writing the results to output file [file] in output directory [dir] *)
let parse_diagnostic start =
  diagnostic ".parsed" ~f:(Parse.Diagnostic.to_file ~start)

open Args

let compile { lex; parse; src_dir; out_dir; files; _ } =
  let map file =
    let src_path = make_path ~dir:src_dir file in
    let apply start lexbuf =
      if lex then lex_diagnostic ~dir:out_dir ~file lexbuf;
      if parse then parse_diagnostic start ~dir:out_dir ~file lexbuf;
      let open Parse in
      lexbuf |> parse ~start |> Result.map_error ~f:string_of_error
    in
    src_path |> Parse.bind ~f:apply |> Result.ignore_m
  in
  files |> List.rev_map ~f:map |> Result.combine_errors_unit

let compile_opt
    ?(lex = default.lex)
    ?(parse = default.parse)
    ?src_dir
    ?out_dir
    files =
  compile { default with lex; parse; src_dir; out_dir; files }
